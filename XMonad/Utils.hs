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
   role
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

-- import Themes

-- Определения

-- Аналог +Control.Monad.when+ для +ManageHook+.

whenH :: Bool -> ManageHook -> ManageHook
whenH condition action = if condition then action else doF id

unlessH :: Bool -> ManageHook -> ManageHook
unlessH condition action = if condition then doF id else action

-- Аналог +XMonad.whenJust+ для +ManageHook+.

whenJustH :: Maybe a -> (a -> ManageHook) -> ManageHook
whenJustH (Just x) action = action x
whenJustH Nothing  _      = doF id

setRootAtom :: String -> String -> X ()
setRootAtom name value = withDisplay (io . setRootAtom' name value)
    where setRootAtom' atom name d = do
                                       a <- internAtom d atom False
                                       rw <- rootWindow d $ defaultScreen d
                                       setTextProperty d rw name a

getRootAtom :: String -> X [String]
getRootAtom name = withDisplay (io . getRootAtom' name)
    where getRootAtom' atom d = do
            a <- internAtom d atom False
            rw <- rootWindow d $ defaultScreen d
            mbtp <- E.catch (Just `fmap` getTextProperty d rw a) (\(E.SomeException e) -> return Nothing)
            case mbtp of
              Just tp -> wcTextPropertyToTextList d tp
              Nothing -> return []

toNewWorkspace :: XPConfig -> X ()
toNewWorkspace xpconfig = do
  x <- inputPrompt xpconfig "New workspace name:"
  whenJust x $ \wksp -> do
    let myWksp = "my-"++wksp
    addHiddenWorkspace myWksp
    windows $ W.shift myWksp

-- Выбрать алгоритм расположения окон по имени.

chooseLayout name = sendMessage $ JumpToLayout name

-- Получить название текущего layout.

getLayout :: X String
getLayout = withWindowSet (\s -> return $ description $ W.layout $ W.workspace $ W.current s)

-- Список окон на текущем рабочем месте.

currentList :: X [Window]
currentList = withWindowSet (\s -> return $ W.integrate' $ W.stack $ W.workspace $ W.current s)

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

-- Получить название текущего рабочего места.

getCurrentWorkspace :: X WorkspaceId
getCurrentWorkspace = withWindowSet (\ws -> return $ W.tag $ W.workspace $ W.current ws)

-- Проверить, является ли окно плавающим.

isFloat :: Window -> X Bool
isFloat w = do
  fls <- withWindowSet (return . W.floating)
  return (w `M.member` fls)

-- xrandr orient = "xrandr -o " ++ orient ++ "; xrandr --output 'VGA-1' --right-of 'DVI-I-1' --mode '1280x1024'"

-- Запустить скрипт +recently-used.py+ для заданных типов файлов. Этот скрипт показывает
-- список последних открывавшихся файлов заданных типов.

{-
recent types = "recently-used.py " ++ unwords (map mime types)
  where
    mime t = fromMaybe "" $ lookup t pairs
    pairs = [("pdf", "application/pdf application/epub+zip"),
             ("djvu", "application/djvu"),
             ("doc", "application/msword"),
             ("png", "image/png")]

Открыть указанную сессию GVim.

vimsession :: String -> X ()
vimsession name = do
  home <- io $ getEnv "HOME"
  let path = home </> ".vim/sessions" </> (name ++ ".vimsession")
  spawn ("gvim -S " ++ path)

Выбрать сессию GVim и открыть её.

vimsessions :: X ()
vimsessions = do
  home <- io $ getEnv "HOME"
  paths <- io $ (concat . fst) `fmap` globDir [compile "*.vimsession"] (home </> ".vim/sessions")
  let sessions = map (dropExtension . takeFileName) paths
  selected <- gridselect myGSConfig $ zip sessions sessions
  whenJust selected vimsession

Запустить один из нескольких текстовых редакторов.

textEditors = do
  let editors = ["gvim", "kate", "gedit"]
  selected <- gridselect myGSConfig $ zip editors editors
  case selected of
    Nothing     -> return ()
    Just "gvim" -> vimsessions
    Just editor -> spawn editor
    -}

role = stringProperty "WM_WINDOW_ROLE"

-- Получить список окон, подходящих под запрос.

matchingWindows :: Query Bool -> X [Window]
matchingWindows query = withWindowSet (return . W.allWindows) >>= filterM (runQuery query)

{-
При закрытии (точнее, unmap) окна — удалить текущее рабочее пространство, если оно осталось пустым.
Пространство "dashboard" — не удалять.

unmapEventHook :: Event -> X All
unmapEventHook (UnmapEvent {}) = do
  current <- getCurrentWorkspace
  when (not $ "dashboard" `isPrefixOf` current || "my-" `isPrefixOf` current) $
      removeEmptyWorkspace
  return (All True)
unmapEventHook _ = return (All True)
-}

-- Запустить одно из действий в зависимости от текущего layout.
-- Первый аргумент — список пар вида (+описание_layout+, +действие_на_этом_layout+)

caseLayoutOf :: [(String, X a)] -> X a -> X a
caseLayoutOf pairs def = do
  layout <- getLayout
  case lookup layout pairs of
    Nothing -> def
    Just x  -> x

ifLayout :: [String] -> X a -> X a -> X a
ifLayout lst yes no = do
  layout <- getLayout
  if layout `elem` lst
    then yes
    else no

-- Если текущее окно — master, не делать ничего; иначе поменять его местами с master-окном.

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

-- Аналог +(=?)+, но проверяет не точное совпадение, а соответствие регулярному выражению.

(~?) :: (Functor f) => f String -> String -> f Bool
q ~? x = fmap (=~ x) q

-- Переключиться на выбранное (с помощью +X.A.GridSelect+) окно из заданного списка.

selectOneWindow :: GSConfig Window -> [Window] -> X ()
selectOneWindow gsconfig wins = do
    titles <- mapM windowTitle wins
    selected <- gridselect gsconfig $ zip titles wins
    whenJust selected $ \w -> do
       focus w
       sendMessage (RestoreMinimizedWin w)
  where
    windowTitle w = show `fmap` getName w

-- Выбрать окно на текущем рабочем месте.

searchInWorkspace :: GSConfig Window -> X ()
searchInWorkspace gsconfig = do
  ws <- currentList
  case ws of
    []  -> return ()
    [x] -> return ()
    _   -> selectOneWindow gsconfig ws

moveToScreen :: ScreenId -> ManageHook
moveToScreen sid = fromWindowOp $ \w -> windows $ \ws ->
  case W.lookupWorkspace sid ws of
    Nothing   -> ws
    Just wksp -> W.shiftWin wksp w ws

changeWorkspaceOn :: ScreenId -> WorkspaceId -> ManageHook
changeWorkspaceOn sid wksp = fromX $ windows $ viewOnScreen sid wksp

-- Делает +ManageHook+ из любой операции с окном.

fromWindowOp :: (Window -> X()) -> ManageHook
fromWindowOp fn = ask >>= \w -> liftX (fn w) >> doF id

-- Делает +ManageHook+ из любого действия.

fromX :: X () -> ManageHook
fromX op = fromWindowOp $ const op

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

