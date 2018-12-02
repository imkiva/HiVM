{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module ClassPath.ControlFlowGraph
    -- * Basic blocks
    -- $basicblocks
  ( BasicBlock
  , basicBlockId
  , basicBlockInstrs
  , BasicBlockId(..)
    -- * Control flow graphs
  , ControlFlowGraph
  , basicBlockById
  , basicBlockByPC
  , cfgNextPC
  , cfgAllBlocks
  , buildControlFlowGraph
  , cfgInstByPC
  , cfgSuccs
  , basicBlockExceptions
    -- * Pretty printing
  , prettyControlFlowGraph
  , prettyBasicBlock
  , prettyInstString
  , prettyBasicBlockId
    -- * Post dominators
  , isImmediatePostDominator
  , getPostDominators
  ) where

import           Control.Arrow               (second)
import           Data.Array
import qualified Data.Foldable               as DF
import           Data.Graph.Inductive
import           Data.IntervalMap.FingerTree (Interval (..), IntervalMap)
import qualified Data.IntervalMap.FingerTree as I
import           Data.List                   as L
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe
import           Prelude                     hiding (rem, (<>))
import           Text.PrettyPrint

import           ClassPath.Common

-- import Debug.Trace
--------------------------------------------------------------------------------
-- Control flow graph construction
data ControlFlowGraph = ControlFlowGraph
  { basicBlockById  :: BasicBlockId -> Maybe BasicBlock
  , basicBlockByPC  :: PC -> Maybe BasicBlock
  , cfgNextPC       :: PC -> Maybe PC
  , cfgAllBlocks    :: [BasicBlock]
  , basicBlockSuccs :: [(BasicBlockId, BasicBlockId)]
  , basicBlockPreds :: BasicBlockId -> [BasicBlockId]
  , cfgSuccs           :: BasicBlockId -> [BasicBlockId]
  , cfgGraph           :: Gr BasicBlockId ()
  , cfgNodeMap         :: NodeMap BasicBlockId
  , cfgIPdoms          :: M.Map BasicBlockId BasicBlockId
  , cfgPdoms           :: M.Map BasicBlockId [BasicBlockId]
  }

entryBlock, exitBlock :: BasicBlock
entryBlock = BasicBlock BasicBlockIdEntry []

exitBlock = BasicBlock BasicBlockIdExit []

-- | Build a control-flow graph from an instruction stream.
--   We assume that the first instruction in the instruction stream is the only
--   external entry point in the sequence (typically, the method entry point).
buildControlFlowGraph :: ExceptionTable -> InstructionStream -> ControlFlowGraph
buildControlFlowGraph exceptionTable stream = cfg
  where
    cfg =
      ControlFlowGraph
        { basicBlockByPC =
            \pc ->
              if pc < firstPC || pc > lastPC
                then Nothing
                else case I.search pc finalBlocks of
                       [(_, bb)] -> Just bb
                       [] -> Nothing
                       _ -> error $ "bbByPC: internal: " ++ "multiple interval match"
        , basicBlockById =
            \case
              BasicBlockId pc -> basicBlockByPC cfg pc
              BasicBlockIdEntry -> Just entryBlock
              BasicBlockIdExit -> Just exitBlock
        , cfgNextPC =
            \pc ->
              case basicBlockByPC cfg pc of
                Nothing -> Nothing
                Just bb ->
                  case basicBlockSuccPC bb pc of
                    Nothing
              -- The given PC has no successor in 'bb', so it must be the leader
              -- PC of the next BB encountered during findNextBB.
                     ->
                      let findNextBB i
                            | i > lastPC = Nothing
                            | otherwise =
                              case basicBlockByPC cfg i of
                                Just _  -> Just i
                                Nothing -> findNextBB (i + 1)
                       in findNextBB (pc + 1)
                    jpc -> jpc
        , cfgAllBlocks = entryBlock : exitBlock : DF.toList finalBlocks
        , basicBlockSuccs =
            (\f -> DF.foldr f [] finalBlocks) $ \bb acc ->
              let newSuccs
                -- Identify a flow edge from bb to x when x's leader is a branch
                -- target of bb's terminator
                   =
                    map
                      (\bt ->
                         case basicBlockByPC cfg bt of
                           Nothing -> error "newSuccs: internal: invalid BBId"
                           Just sbb -> (basicBlockId bb, basicBlockId sbb))
                      (brTargets $ terminatorPC bb) ++
                    -- Identify a flow edge from bb to x when bb's terminator
                    -- doesn't break the control flow path x's leader is the
                    -- instruction successor of bb's terminator.
                    case do let tpc = terminatorPC bb
                            breaksCFP <- breaksControlFlow `fmap` basicBlockInstByPC bb tpc
                            if breaksCFP
                              then Nothing
                              else basicBlockByPC cfg =<< cfgNextPC cfg tpc of
                      Nothing  -> []
                      Just nbb -> [(basicBlockId bb, basicBlockId nbb)]
            -- NB: Hook up entry/exit blocks here
               in (if leaderPC bb == firstPC
                     then ((BasicBlockIdEntry, basicBlockId bb) :)
                     else id) $
                  case newSuccs of
                    [] -> (basicBlockId bb, BasicBlockIdExit) : acc
                    _  -> newSuccs ++ acc
        , basicBlockPreds =
            \bbid ->
              case basicBlockById cfg bbid of
                Nothing -> error "CFG.preds: invalid BBId"
                Just _ -> map fst $ filter ((== bbid) . snd) $ basicBlockSuccs cfg
        , cfgSuccs =
            \bbid ->
              case basicBlockById cfg bbid of
                Nothing -> error "CFG.succs: invalid BBId"
                Just _ -> map snd $ filter ((== bbid) . fst) $ basicBlockSuccs cfg
        , cfgGraph = gr
        , cfgNodeMap = nm
        , cfgIPdoms = M.fromList . map pairFromInts . ipdom $ gr
        , cfgPdoms = M.fromList . map adjFromInts . pdom $ gr
        }
    (gr, nm) = buildGraph cfg
    -- post-dominators are just dominators where the exit is the root
    ipdom = flip iDom (fst $ mkNode_ nm BasicBlockIdExit) . grev
    pdom = flip dom (fst $ mkNode_ nm BasicBlockIdExit) . grev
    pairFromInts (n, n') = (lkup n, lkup n')
    lkup n = fromMaybe (modError "found node not in graph") $ lab gr n
    adjFromInts (n, ns) = (lkup n, map lkup ns)
    finalBlocks = blocks $ foldr process (BasicBlockInfo I.empty (BasicBlock (BasicBlockId firstPC) []) False) rinsts
    --
    process i@(pc, _) bbi = processInst (isLastBlockBranch bbi || isBrTarget pc) (isBrInst pc) firstPC lastPC i bbi
    -- instruction sequence fixup & reordering
    rinsts@((lastPC, _):_) = reverse insts
    insts@((firstPC, _):_) = map fixup $ filter valid $ assocs stream
      where
        valid (_, Just {}) = True
        valid _            = False
        fixup (pc, Just i) = (pc, i)
        fixup _            = error "impossible"
    --
    isBrInst pc = not . null $ brTargets pc
    isBrTarget pc = pc `elem` concat (M.elems btm)
    btm = makeBrTargetMap exceptionTable stream
    brTargets pc = fromMaybe [] $ M.lookup pc btm

--  let dot = cfgToDot extbl cfg "???" in
--  trace ("dot:\n" ++ dot) $
-- | We want to keep the node map around to look up specific nodes
--   later, even though I think we only do that once
buildGraph :: ControlFlowGraph -> (Gr BasicBlockId (), NodeMap BasicBlockId)
buildGraph cfg = (mkGraph ns es, nm)
  where
    (ns, nm) = mkNodes new (map basicBlockId . cfgAllBlocks $ cfg)
    es = fromMaybe (modError "edge with unknown nodes") $ mkEdges nm edgeTriples
    edgeTriples :: [(BasicBlockId, BasicBlockId, ())] -- ready to become UEdges
    edgeTriples = map (\(n1, n2) -> (n1, n2, ())) (basicBlockSuccs cfg)

-- | @isImmediatePostDominator g x y@ returns @True@ if @y@
--   immediately post-dominates @x@ in control-flow graph @g@.
isImmediatePostDominator :: ControlFlowGraph -> BasicBlockId -> BasicBlockId -> Bool
isImmediatePostDominator cfg bb bb' = (== Just bb') . M.lookup bb . cfgIPdoms $ cfg

-- | Calculate the post-dominators of a given basic block.
getPostDominators :: ControlFlowGraph -> BasicBlockId -> [BasicBlockId]
getPostDominators cfg bb = M.findWithDefault [] bb (cfgPdoms cfg)

--------------------------------------------------------------------------------
-- $basicblocks
--
-- Our notion of basic block is fairly standard: a maximal sequence of
-- instructions that that can only be entered at the first of them and existed
-- only from the last of them.  I.e., a contiguous sequence of instructions that
-- neither branch or are themselves branch targets.
--
-- The first instruction in a basic block (i.e., a "leader") may be (a) a method
-- entry point, (b) a branch target, or (c) an instruction immediately following
-- a branch/return.
--
-- To identify constituent basic blocks of a given instruction sequence, we
-- identify leaders and then, for each leader, include in its basic block all
-- instructions, in order, that intervene it and the next leader or the end of
-- the instruction sequence, whichever comes first.
-- | Identifies basic blocks by their position
--   in the instruction stream, or by the special
--   `BBIdEntry` or `BBIdExit` constructors.
data BasicBlockId
  = BasicBlockIdEntry
  | BasicBlockIdExit
  | BasicBlockId PC
  deriving (Eq, Ord, Show)

instance Enum BasicBlockId where
  toEnum 0 = BasicBlockIdEntry
  toEnum 1 = BasicBlockIdExit
  toEnum n = BasicBlockId (fromIntegral n - 2)
  fromEnum BasicBlockIdEntry = 0
  fromEnum BasicBlockIdExit  = 1
  fromEnum (BasicBlockId n)  = fromIntegral n + 2

-- | A basic block consists of an identifier and
--   the instructions contained in that block.
data BasicBlock = BasicBlock
  { basicBlockId     :: BasicBlockId
  , basicBlockInstrs :: [(PC, Instruction)]
  } deriving (Show)

data BasicBlockInfo = BasicBlockInfo
  { blocks            :: IntervalMap PC BasicBlock
  , currentBlock      :: BasicBlock
  , isLastBlockBranch :: Bool -- True iff the last instruction examined was a
                           -- branch/return for bb splitting purposes
  }

-- NB: We're punting on explicit bb splits/cfg edges on thrown exceptions and
-- instructions that can raise exceptions in the presence of a handler for the
-- time being.
processInst ::
     Bool -- | Is the given instruction a leader?
  -> Bool -- | Is the given instruction a branch?
  -> PC -- | Value of first PC in instruction sequence
  -> PC -- | Value of last PC in instruction sequence
  -> (PC, Instruction) -- | Current PC and instruction
  -> BasicBlockInfo -- | BB accumulator
  -> BasicBlockInfo
processInst isLeader isBranchInst firstPC lastPC (pc, inst) bbi =
  let bbi' =
        if isLeader
          then newBB
          else noNewBB
   in bbi'
        { isLastBlockBranch = isBranchInst || breaksControlFlow inst
        , blocks =
            if pc == lastPC
              then mk (currentBlock bbi') `I.union` blocks bbi'
              else blocks bbi'
        }
  where
    mk bb =
      let bb' = bb {basicBlockInstrs = reverse (basicBlockInstrs bb)}
       in I.singleton (basicBlockInterval bb') bb'
    addInst bb = bb {basicBlockInstrs = (pc, inst) : basicBlockInstrs bb}
    noNewBB = bbi {currentBlock = addInst (currentBlock bbi)}
    newBB
      | pc == firstPC = noNewBB
      | otherwise =
        bbi
          { blocks = mk (currentBlock bbi) `I.union` blocks bbi
          , currentBlock = addInst (BasicBlock (BasicBlockId pc) [])
          }

--------------------------------------------------------------------------------
-- Branch target calculation
--
-- Most branch targets can be determined by simple inspection of a given
-- instruction.  A notable (and unfortunate) exception is determining the
-- targets of a 'ret' instruction.  Since the address to which a ret jumps is a
-- first-class value, we need to do some dataflow analysis to figure out the
-- potential targets.  Note that we make a few simplifying concessions, e.g.,
-- that jsr targets are always astores and that the bytecode we're analyzing
-- passes bytecode verification (e.g., to ensure that two subroutines may not
-- share a 'ret', and so forth).
--
-- We do a (fairly sparse) abstract execution of the method, tracking the state
-- pertaining to jsr/ret pairings along all execution paths.
makeBrTargetMap :: ExceptionTable -> InstructionStream -> Map PC [PC]
makeBrTargetMap exceptionTable stream = foldr f M.empty stream'
  where
    f (pc, i) acc = maybe acc (\v -> M.insert pc v acc) $ getBranchPCs i
    stream'@((firstPC, _):_) = assocs stream
    --
    getBranchPCs Nothing = Nothing
    getBranchPCs (Just i) =
      case i of
        Goto pc -> Just [pc]
        If_acmpeq pc -> Just [pc]
        If_acmpne pc -> Just [pc]
        If_icmpeq pc -> Just [pc]
        If_icmpne pc -> Just [pc]
        If_icmplt pc -> Just [pc]
        If_icmpge pc -> Just [pc]
        If_icmpgt pc -> Just [pc]
        If_icmple pc -> Just [pc]
        Ifeq pc -> Just [pc]
        Ifne pc -> Just [pc]
        Iflt pc -> Just [pc]
        Ifge pc -> Just [pc]
        Ifgt pc -> Just [pc]
        Ifle pc -> Just [pc]
        Ifnonnull pc -> Just [pc]
        Ifnull pc -> Just [pc]
        Jsr pc -> Just [pc]
        Ret {} ->
          let xfer = returnTargetXfer exceptionTable stream
           in case doFlow xfer M.empty [(Just firstPC, [])] [] of
                [] -> error "Internal: dataflow analysis yielded no targets for ret"
                bs -> Just $ map snd bs
        Lookupswitch dflt tgts -> Just $ dflt : map snd tgts
        Tableswitch dflt _ _ tgts -> Just $ dflt : tgts
        _ -> Nothing

type XferF state acc = acc -> state -> (acc, [state])

-- | A simple worklist-based propagator for dataflow analysis
doFlow ::
     ( Eq state
     , Ord state {-, Show state-}
     )
  => XferF state acc -- | the state transfer function
  -> Map state () -- | the seen states map
  -> [state] -- | states worklist
  -> acc -- | accumulated dataflow result
  -> acc
doFlow _ _ [] acc = acc
doFlow xfer seen (curr:rem) !acc =
  let (acc', new') = filter (`M.notMember` seen) `second` xfer acc curr
   in doFlow xfer (foldr (`M.insert` ()) seen new') (rem ++ new') acc'

--    trace ("doFlow step: worklist is: " ++ show (rem ++ new)) $
-- We represent the dataflow state before execution of an instruction at PC 'p'
-- by (Just p, L), where L is a relation between local variable indices and
-- return address values.  States of the form (Nothing, _) denote termination.
type BranchTargetState = (Maybe PC, [(LocalVariableIndex, PC)])

returnTargetXfer :: ExceptionTable -> InstructionStream -> XferF BranchTargetState [(PC, PC)]
returnTargetXfer _ _ acc (Nothing, _) = (acc, [])
returnTargetXfer exceptionTable stream acc (Just pc, localVars) = xfer (lookupStream pc)
  where
    succPC = safeNextPcPrim stream
    ssuccPC = fromMaybe (error "returnTargetXfer: invalid succPC") . succPC
    lookupStream p = fromMaybe (error $ "returnTargetXfer: Invalid inst @ " ++ show p) (stream ! p)
    --
    xfer (Jsr label) =
      case lookupStream label of
        Astore i -> (acc, [(succPC label, (i, ssuccPC pc) : localVars)])
                  -- next state: the call of the referent subroutine (after
                  -- its astore leader) with the successor of the jsr stored
                  -- into the local specified by the astore
        _ -> error "returnTargetXfer: Assumed jsr targets are always Astore"
    --
    xfer (Ret k) =
      case lookup k localVars of
        Just q  -> ((pc, q) : acc, [(Just q, localVars \\ [(k, q)])])
                 -- inst(p) = ret k && L(k) = q; this is where we detect that
                 -- this ret instruction branches to q along the control flow
                 -- path we've been following.  Record this fact in 'acc' and
                 -- then follow the jump back to the jsr succ.
        Nothing -> error $ "returnTargetXfer: No retaddr at lidx " ++ show k
    --
    xfer inst
      | canThrowException inst
        -- continue dataflow analysis at the next instruction (unless we're
        -- terminating this cfp here) and also at each exception handler that
        -- /may/ fire upon execution of the current instruction
       =
        ( acc
        , let getHandlerPC (ExceptionTableEntry _ _ h _) = h
              ts = map ((, localVars) . Just . getHandlerPC) . filter (exceptionCoversPC pc) $ exceptionTable
           in if breaksControlFlow inst
                then ts -- terminate this cfp, but still evaluate exception cfps
                else (succPC pc, localVars) : ts -- next inst + exception cfps
         )
    --
    xfer _ = (acc, [(succPC pc, localVars)]) -- continue

--------------------------------------------------------------------------------
-- Utility functions
leaderPC :: BasicBlock -> PC
leaderPC BasicBlock {basicBlockInstrs = []} = error "internal: leaderPC on empty BB"
leaderPC bb = fst . head . basicBlockInstrs $ bb

terminatorPC :: BasicBlock -> PC
terminatorPC BasicBlock {basicBlockInstrs = []} = error "internal: terminatorPC on empty BB"
terminatorPC bb = fst . last . basicBlockInstrs $ bb

-- | Fetch an instruction from a Control Flow Graph by position.
cfgInstByPC :: ControlFlowGraph -> PC -> Maybe Instruction
cfgInstByPC cfg pc = basicBlockByPC cfg pc >>= flip basicBlockInstByPC pc

basicBlockInstByPC :: BasicBlock -> PC -> Maybe Instruction
basicBlockInstByPC bb pc = lookup pc (basicBlockInstrs bb)

basicBlockInterval :: BasicBlock -> Interval PC
basicBlockInterval bb = Interval (leaderPC bb) (terminatorPC bb)

basicBlockPCs :: BasicBlock -> [PC]
basicBlockPCs = map fst . basicBlockInstrs

basicBlockSuccPC :: BasicBlock -> PC -> Maybe PC
basicBlockSuccPC bb pc =
  case drop 1 $ dropWhile (/= pc) $ map fst $ basicBlockInstrs bb of
    []         -> Nothing
    (succPC:_) -> Just succPC

exceptionCoversPC :: PC -> ExceptionTableEntry -> Bool
exceptionCoversPC pc (ExceptionTableEntry s e _ _) = pc >= s && pc <= e

basicBlockExceptions :: ExceptionTable -> BasicBlock -> ExceptionTable
basicBlockExceptions exceptionTable block =
  nub $ concatMap (\pc -> filter (exceptionCoversPC pc) exceptionTable) (basicBlockPCs block)

modError :: String -> a
modError msg = error $ "ControlFlowGraph: " ++ msg

--------------------------------------------------------------------------------
-- Pretty-printing
prettyControlFlowGraph :: ControlFlowGraph -> String
prettyControlFlowGraph cfg = "ControlFlowGraph {\n" ++ showOnNewLines 2 prettifiedBBs ++ "\n}"
  where
    prettifiedBBs = (map prettyBasicBlock . cfgAllBlocks) cfg

prettyBasicBlock :: BasicBlock -> String
prettyBasicBlock bb = "BasicBlock(" ++ show (basicBlockId bb) ++ "):\n" ++ instString
  where
    instString = showOnNewLines 4 (map (\(pc, inst) -> show pc ++ ": " ++ prettyInstString inst) (basicBlockInstrs bb))

prettyInstString :: Instruction -> String
prettyInstString (Invokevirtual (JavaClassType className) methodId) =
  "Invokevirtual " ++ prettyMethodIdString className methodId
prettyInstString (Invokespecial (JavaClassType className) methodId) =
  "Invokespecial " ++ prettyMethodIdString className methodId
prettyInstString (Invokestatic className methodId) = "Invokestatic " ++ prettyMethodIdString className methodId
prettyInstString (Invokeinterface className methodId) = "Invokeinterface " ++ prettyMethodIdString className methodId
prettyInstString (Getfield fieldId) = "Getfield " ++ prettyFieldId fieldId
prettyInstString (Putfield fieldId) = "Putfield " ++ prettyFieldId fieldId
prettyInstString (New className) = "New " ++ slashesToDots (unpackClassName className)
prettyInstString (Ldc (String s)) = "Ldc (String " ++ "\"" ++ s ++ "\")"
prettyInstString (Ldc (ClassRef className)) = "Ldc (ClassRef " ++ slashesToDots (unpackClassName className) ++ ")"
prettyInstString (Getstatic fieldId) = "Getstatic " ++ prettyFieldId fieldId
prettyInstString (Putstatic fieldId) = "Putstatic " ++ prettyFieldId fieldId
prettyInstString i = show i

prettyMethodIdString :: JavaClassName -> MethodId -> String
prettyMethodIdString className methodId = slashesToDots (unpackClassName className) ++ "." ++ methodIdName methodId

prettyBasicBlockId :: BasicBlockId -> Doc
prettyBasicBlockId bbid =
  case bbid of
    BasicBlockIdEntry -> "BB%entry"
    BasicBlockIdExit  -> "BB%exit"
    BasicBlockId pc   -> "BB%" <> int (fromIntegral pc)

--------------------------------------------------------------------------------
-- Instances
instance Show ControlFlowGraph where
  show cfg = "ControlFlowGraph { allBBs = " ++ show (cfgAllBlocks cfg) ++ " }"

instance Eq ControlFlowGraph where
  cfg1 == cfg2 = cfgAllBlocks cfg1 == cfgAllBlocks cfg2 && basicBlockSuccs cfg1 == basicBlockSuccs cfg2

instance Eq BasicBlock where
  bb1 == bb2 = basicBlockId bb1 == basicBlockId bb2
