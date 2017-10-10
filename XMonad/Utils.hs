module XMonad.Utils
  (
   getCurrentWorkspace,
   searchInWorkspace, enshureMaster,
   caseLayoutOf, ifLayout, currentList, windowsOnWorkspace, isFloat,
   matchingWindows, selectOneWindow,
   moveToScreen, changeWorkspaceOn,
   toNewWorkspace,
   createAndMove,
   chooseLayout,
   (~?),
   fromX, fromWindowOp,
   whenH, unlessH, whenJustH,
   setRootAtom, getRootAtom,
   role,
   isMaster, checkIsMaster, ifMaster
  ) where

import Control.Monad (filterM, when)
import qualified Control.Exception as E
import System.FilePath.Glob
import System.FilePath
import System.Environment (getEnv)
import Data.Maybe
import Data.Monoid
import Data.List
import qualified Data.Map as M

import Graphics.X11.Xlib.Extras
import Text.Regex.Posix ((=~))

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Util.NamedWindows
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Minimize

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.OnScreen
import XMonad.Prompt
import XMonad.Prompt.Input

-- | An analogue of @Control.Monad.when@ for @ManageHook@
whenH :: Bool -> ManageHook -> ManageHook
whenH condition action = if condition then action else doF id

-- | An analogue of @Control.Monad.unless@ for @ManageHook@
unlessH :: Bool -> ManageHook -> ManageHook
unlessH condition action = if condition then doF id else action

-- | An analogue of @XMonad.whenJust@ for @ManageHook@.
whenJustH :: Maybe a -> (a -> ManageHook) -> ManageHook
whenJustH (Just x) action = action x
whenJustH Nothing  _      = doF id

-- | Set X11 root window atom.
setRootAtom :: String -> String -> X ()
setRootAtom name value = withDisplay (io . setRootAtom' name value)
    where setRootAtom' atom name d = do
                                       a <- internAtom d atom False
                                       rw <- rootWindow d $ defaultScreen d
                                       setTextProperty d rw name a

-- | Get X11 root window atom.
getRootAtom :: String -> X [String]
getRootAtom name = withDisplay (io . getRootAtom' name)
    where getRootAtom' atom d = do
            a <- internAtom d atom False
            rw <- rootWindow d $ defaultScreen d
            mbtp <- E.catch (Just `fmap` getTextProperty d rw a) (\(E.SomeException e) -> return Nothing)
            case mbtp of
              Just tp -> wcTextPropertyToTextList d tp
              Nothing -> return []

-- | Create new workspace and move current window to it.
toNewWorkspace :: XPConfig -> X ()
toNewWorkspace xpconfig = do
  x <- inputPrompt xpconfig "New workspace name:"
  whenJust x $ \wksp -> do
    let myWksp = "my-"++wksp
    addHiddenWorkspace myWksp
    windows $ W.shift myWksp

-- | Select widow layout by name.
chooseLayout name = sendMessage $ JumpToLayout name

-- | Get name of current layout.
getLayout :: X String
getLayout = withWindowSet (\s -> return $ description $ W.layout $ W.workspace $ W.current s)

-- | List of windows in current workspace.
currentList :: X [Window]
currentList = withWindowSet (\s -> return $ W.integrate' $ W.stack $ W.workspace $ W.current s)

-- | List of windows in given workspace.
windowsOnWorkspace :: WorkspaceId -> X [Window]
windowsOnWorkspace name =
    withWindowSet $ \s -> do
      let ws = W.workspaces s
          mbWksp = case [w | w <- ws, W.tag w == name] of
                    [w] -> Just w
                    _ -> Nothing
      case mbWksp of
        Nothing -> return []
        Just wksp -> return $ W.integrate' $ W.stack wksp

-- | Get name of current workspace.
getCurrentWorkspace :: X WorkspaceId
getCurrentWorkspace = withWindowSet (\ws -> return $ W.tag $ W.workspace $ W.current ws)

-- | Check if the window is floating
isFloat :: Window -> X Bool
isFloat w = do
  fls <- withWindowSet (return . W.floating)
  return (w `M.member` fls)

role = stringProperty "WM_WINDOW_ROLE"

-- | Get the list of windows matching to query.
matchingWindows :: Query Bool -> X [Window]
matchingWindows query = withWindowSet (return . W.allWindows) >>= filterM (runQuery query)

-- Запустить одно из действий в зависимости от текущего layout.
-- Первый аргумент — список пар вида (+описание_layout+, +действие_на_этом_layout+)

-- | Run one of actions depending of current layout.
caseLayoutOf :: [(String, X a)] -- ^ [(layout name, action)]
              -> X a -- ^ Default action
              -> X a
caseLayoutOf pairs def = do
  layout <- getLayout
  case lookup layout pairs of
    Nothing -> def
    Just x  -> x

-- | Run one of actions depending on current layout
ifLayout :: [String] -> X a -> X a -> X a
ifLayout lst yes no = do
  layout <- getLayout
  if layout `elem` lst
    then yes
    else no

-- | If the current window is master, do nothing;
-- otherwise, swap it with master window.
enshureMaster :: X ()
enshureMaster =
  withFocused $ \w -> do
    ws <- currentList
    case ws of
      [] -> return ()
      _  -> let master = head ws
            in  if master == w
                  then return ()
                  else windows W.swapMaster

-- | An analogue of @(=?)@, but checks regular expression match
(~?) :: (Functor f) => f String -> String -> f Bool
q ~? x = fmap (=~ x) q

-- | Switch to selected (by using @X.A.GridSelect@) window from specified list.
selectOneWindow :: GSConfig Window -> [Window] -> X ()
selectOneWindow gsconfig wins = do
    titles <- mapM windowTitle wins
    selected <- gridselect gsconfig $ zip titles wins
    whenJust selected $ \w -> do
       focus w
       sendMessage (RestoreMinimizedWin w)
  where
    windowTitle w = show `fmap` getName w

-- | Switch to selected window in current workspace.
searchInWorkspace :: GSConfig Window -> X ()
searchInWorkspace gsconfig = do
  ws <- currentList
  case ws of
    []  -> return ()
    [x] -> return ()
    _   -> selectOneWindow gsconfig ws

-- | Move window to specified screen
moveToScreen :: ScreenId -> ManageHook
moveToScreen sid = fromWindowOp $ \w -> windows $ \ws ->
  case W.lookupWorkspace sid ws of
    Nothing   -> ws
    Just wksp -> W.shiftWin wksp w ws

changeWorkspaceOn :: ScreenId -> WorkspaceId -> ManageHook
changeWorkspaceOn sid wksp = fromX $ windows $ viewOnScreen sid wksp

-- | Make a @ManageHook@ from any action on @Window@.
fromWindowOp :: (Window -> X()) -> ManageHook
fromWindowOp fn = ask >>= \w -> liftX (fn w) >> doF id

-- | Make a @ManageHook@ from any @X@ action.
fromX :: X () -> ManageHook
fromX op = fromWindowOp $ const op

-- | Create new workspace and move current window to it.
createAndMove :: Bool -> (Maybe ScreenId) -> WorkspaceId -> ManageHook
createAndMove jump mbSID wksp = do
  liftX (addHiddenWorkspace wksp)
  case (mbSID, jump) of
    (Nothing,  True)  -> fromX $ windows (W.view wksp)
    (Just sid, False) -> doF (onlyOnScreen sid wksp)
    (Just sid, True)  -> doF (viewOnScreen sid wksp)
    otherwise         -> doF id
  w <- ask
  doF (W.shiftWin wksp w) :: ManageHook

-- | Is the focused window the \"master window\" of the current workspace?
isMaster :: Query Bool
isMaster = ask >>= (\w -> liftX $ withWindowSet $ \ws -> return $ Just w == master ws)
  where
    master :: WindowSet -> Maybe Window
    master ws = 
        case W.integrate' $ W.stack $ W.workspace $ W.current ws of
             [] -> Nothing
             (x:xs) -> Just x

checkIsMaster :: X Bool
checkIsMaster =
  withWindowSet $ \ss -> do
    case W.peek ss of
      Just w -> runQuery isMaster w
      Nothing -> return False

ifMaster :: X a -> X a -> X a
ifMaster true false = do
  master <- checkIsMaster
  if master
    then true
    else false

