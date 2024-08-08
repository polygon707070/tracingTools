{-# LANGUAGE ViewPatterns, DeriveDataTypeable #-}

-- Transition graph drawing functions
module GraphDraw(RGraphDraw,
                 GraphDraw(gGraph),
                 GraphDrawCB(..),
                 GraphDrawGr,
                 GAnnotation(..),
                 GMode(..),
                 GCursor(..),
                 AnnotLoc(..),
                 EndStyle(..),
                 GNodeId,
                 GEdgeId,
                 GC(..),

                 graphDrawNew,
                 graphDrawDefaultCB,
                 graphDrawConnect, 
                 graphDrawDisconnect,
                 graphDrawWidget,
                 graphDrawSetCB,
                 graphDrawFitToScreen,
                 graphDrawSetMode,

                 graphDrawInsertNode,
                 graphDrawDeleteNode,
                 graphDrawGetNodeCoords,
                 graphDrawSetNodeCoords,
                 graphDrawGetNodeAnnots,
                 graphDrawSetNodeAnnots,
                 graphDrawSetNodeStyle,

                 graphDrawInsertEdge,
                 graphDrawDeleteEdge,
                 graphDrawUpdateEdgeEnds,
                 graphDrawGetEdgeAnnots,
                 graphDrawSetEdgeAnnots,
                 graphDrawSetEdgeCorners,
                 graphDrawSetEdgeVisible,
                 graphDrawSetEdgeStyle,
                 graphDrawStraightenEdge,

                 graphDrawAutoLayout,
                 graphDrawToString,
                 graphDrawFromString,
                 graphDrawSetGr,

                 graphDrawGetNodesAtLocation) where

import Prelude hiding (catch)
import GHC.Word
import Data.IORef
import Data.List
import Data.Maybe
import Data.Data
import Data.Generics.Text

import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.GC as G
import qualified Graphics.UI.Gtk.Gdk.Events as Event
import Graphics.UI.Gtk.Gdk.EventM
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.GraphViz.Printing as GVP
import qualified Data.GraphViz.Types as GVT
import qualified Data.GraphViz.Types.Graph as GVTG
import qualified Data.GraphViz.Types.Canonical as GVTC
import qualified Data.Text.Lazy as T
import Data.GraphViz.Parsing
import Data.GraphViz.Commands
import Data.GraphViz.Attributes.Complete
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree (Gr)
import Control.Exception
import Debug.Trace

----------------------------------------------------------
-- Constants
----------------------------------------------------------

loopWidth  = 50
loopHeight = 20


backgroundStyle   = GC {gcFG = (65535, 65535, 65535), gcLW=0, gcLS=True}
activeCornerStyle = GC {gcFG = (45000, 0, 0),         gcLW=2, gcLS=True}
targetStyle       = GC {gcFG = (15000, 15000, 15000), gcLW=2, gcLS=True}
ghostStyle        = GC {gcFG = (35000, 35000, 35000), gcLW=2, gcLS=True}

pixmapWidth = 3000
pixmapHeight = 5000

viewportWidth = 200

minScaling = 0.05
maxScaling = 2.00
fitMargin = 25

arrowAngle = 0.4
arrowLen = 15

edgeProximityThreshold = 4
cornerProximityThreshold = 5
defaultFontSize = 12

defNodeRadius = 16 :: Double
activeCornerRadius = 4 :: Double
targetRadius = 10

----------------------------------------------------------
-- Types
----------------------------------------------------------


type GNodeId = Int
type GEdgeId = Int

-- Location of annotation wrt annotated edge or node 
data AnnotLoc = AnnotRight
              | AnnotLeft
              deriving (Eq, Show, Typeable, Data)

data EndStyle = EndNone
              | EndDiamond
              | EndArrow
              deriving (Eq, Show, Typeable, Data)

data GC = GC { gcFG :: (Word16,Word16,Word16)
             , gcLW :: Int
             , gcLS :: Bool}
             deriving (Typeable,Data)

gcToGCV :: GC -> G.GCValues
gcToGCV (GC (r,g,b) lw ls) = G.newGCValues {G.foreground = G.Color r g b, G.lineWidth = lw, G.lineStyle = if ls then G.LineSolid else G.LineOnOffDash}

gcvToGC :: G.GCValues -> GC
gcvToGC gcv = GC { gcFG = (r,g,b)
                 , gcLW = G.lineWidth gcv
                 , gcLS = case G.lineStyle gcv of
                               G.LineSolid -> True
                               _           -> False}
              where G.Color r g b = G.foreground gcv

-- Edge or node annotation
data GAnnotation = GAnnotation {
    annotText      :: String,
    annotLoc       :: AnnotLoc,
    annotTextStyle :: GC
} deriving (Typeable, Data)

-- Graph node
data GNode = GNode {
    gnAnnot     :: [GAnnotation],
    gnCoords    :: (Double,Double),  -- node coordinates
    gnRScale    :: Double,           -- node radius scaling factor
    gnLineStyle :: GC,
    gnAreaStyle :: GC
} deriving (Typeable,Data)

-- Graph edge
data GEdge = GEdge {
    geId        :: GEdgeId,
    geAnnot     :: [GAnnotation],
    geCorners   :: [(Double,Double)], -- coordinates of edge corners
    geLineStyle :: GC,
    geEndStyle  :: EndStyle,
    geVisible   :: Bool
} deriving (Typeable,Data)

instance Eq GEdge where
    (==) e1 e2 = geId e1 == geId e2

data GCursor = CursorGhost
             | CursorTarget

-- Widget mode set by the client
data GMode = GNormal          -- normal mode: choose and drag states and edges
           | GLocate GCursor  -- user is choosing an object in the graph.  Normal operations are disabled


data GState = GIdle
            | GNodeDrag GNodeId GraphDrawGr
            | GCornerDrag (LEdge GEdge) (Either Int Int)

data GraphDrawCB = GraphDrawCB {
    -- normal-mode callbacks
    onNodeRightClick  :: GNodeId                                         -> IO (),
    onEdgeLeftClick   :: (Int, Int, GEdgeId)                             -> IO (),
    onEdgeRightClick  :: (Int, Int, GEdgeId)                             -> IO (),
    onEmptyRightClick :: (Double, Double)                                -> IO (),
    onNodeLeftClick   :: GNodeId                                         -> IO (),
    onNodeMoved       :: GNodeId -> (Double, Double) -> (Double, Double) -> IO (),
    onEdgeMoved       ::                                                    IO (),
    -- locate-mode callbacks
    onLocateEmpty     :: G.MouseButton -> (Double, Double)               -> IO (),
    onLocateNode      :: G.MouseButton -> GNodeId                        -> IO ()
}

graphDrawDefaultCB = GraphDrawCB { onNodeRightClick    = (\_ -> return ())
                                 , onEdgeRightClick    = (\_ -> return ())
                                 , onEdgeLeftClick     = (\_ -> return ())
                                 , onEmptyRightClick   = (\_ -> return ())
                                 , onNodeLeftClick     = (\_ -> return ())
                                 , onNodeMoved         = (\_ _ _ -> return ())
                                 , onEdgeMoved         = return ()
                                 , onLocateEmpty       = (\_ _ -> return ())
                                 , onLocateNode        = (\_ _ -> return ())}


type GraphDrawGr = Gr GNode GEdge

-- Graph widget
data GraphDraw = GraphDraw {
    gConnected      :: Bool,
    gBox            :: G.VBox,
    gScrolledWindow :: G.ScrolledWindow,
    gDrawingArea    :: G.DrawingArea,
    gScale          :: G.HScale,
    gPixmap         :: Maybe G.Pixmap,

    gGraph          :: GraphDrawGr,
    gMode           :: GMode,
    gState          :: GState,
    gScaling        :: Double,

    -- The last mouse position inside the graph area
    gMousePosition  :: (Double, Double),
    -- Corner near mouse position
    gActiveCorner   :: Maybe ((LEdge GEdge), Int),
    -- Node at mouse position
    gActiveNode     :: Maybe GNodeId,
    -- Edge at mouse position
    gActiveEdge     :: Maybe GEdgeId,

    gCB             :: GraphDrawCB
}

type RGraphDraw = IORef GraphDraw

gNode :: GraphDrawGr -> GNodeId -> GNode
gNode g n = fromJust $ lab g n

gEdge :: GraphDrawGr -> GEdgeId -> (GNodeId, GNodeId, GEdge)
gEdge g id = fromJust $ find (\(n1, n2, e) -> (geId e == id)) (labEdges g)

----------------------------------------------------------
-- Interface methods
----------------------------------------------------------

graphDrawNew :: IO RGraphDraw
graphDrawNew = do
    -- vbox to store graph drawing area and scaling control
    vbox <- G.vBoxNew False 0
    G.widgetShow vbox

    -- scrolled window for the graph
    scroll <- G.scrolledWindowNew Nothing Nothing 
    G.widgetShow scroll
    G.boxPackStart vbox scroll G.PackGrow 0

    -- viewport inside scrolled window
    scrolladj <- G.adjustmentNew 0 0 100 5 30 30
    viewport <- G.viewportNew scrolladj scrolladj
    G.widgetSetSizeRequest viewport viewportWidth (-1) 
    G.widgetShow viewport
    G.containerAdd scroll viewport

    -- graph drawing area
    area <- G.drawingAreaNew
    G.widgetShow area
    G.containerAdd viewport area
    G.widgetSetSizeRequest area pixmapWidth pixmapHeight

    -- graph scaling control
    adj <- G.adjustmentNew 100 5 210 1 10 10
    scale <- G.hScaleNew adj
    G.scaleSetDigits scale 0
    G.scaleSetValuePos scale G.PosLeft
    G.scaleSetDrawValue scale True
    G.rangeSetValue scale 100
    G.widgetShow scale

    G.boxPackStart vbox scale G.PackNatural 0

    ref <- newIORef $ GraphDraw { gConnected      = False
                                , gBox            = vbox
                                , gGraph          = empty
                                , gScrolledWindow = scroll
                                , gDrawingArea    = area
                                , gScale          = scale
                                , gPixmap         = Nothing
                                , gMode           = GNormal
                                , gState          = GIdle
                                , gScaling        = 1
                                , gMousePosition  = (0,0)
                                , gActiveCorner   = Nothing
                                , gActiveNode     = Nothing
                                , gActiveEdge     = Nothing
                                , gCB             = graphDrawDefaultCB}

    G.onRealize area (updatePixmap ref)
    G.on area G.motionNotifyEvent (mouseMove ref)
    G.widgetAddEvents area [G.PointerMotionMask,G.PointerMotionHintMask]
    G.on area G.exposeEvent (updateGraph ref)
    G.on area G.buttonPressEvent (graphMouseClick ref)
    G.on area G.buttonReleaseEvent (graphMouseClick ref)

    hsb <- G.scrolledWindowGetHScrollbar scroll
    G.on (fromJust hsb) G.valueChanged (updatePixmap ref)
    vsb <- G.scrolledWindowGetVScrollbar scroll
    G.on (fromJust vsb) G.valueChanged (updatePixmap ref)
    G.on scale G.changeValue (\_ scale -> do {changeScale ref scale; forceGraphUpdate ref; return False;})

    forceGraphUpdate ref
    return ref

graphDrawConnect :: RGraphDraw -> IO ()
graphDrawConnect ref = do
    graph <- readIORef ref
    writeIORef ref $ graph {gConnected = True}
    forceGraphUpdate ref

graphDrawDisconnect :: RGraphDraw -> IO ()
graphDrawDisconnect ref = do
    graph <- readIORef ref
    writeIORef ref $ graph {gConnected = False}

graphDrawWidget :: RGraphDraw -> IO G.Widget
graphDrawWidget ref = do
    graph <- readIORef ref
    return $ G.toWidget $ gBox graph

graphDrawSetCB :: RGraphDraw -> GraphDrawCB -> IO ()
graphDrawSetCB ref cb = do
    graph <- readIORef ref
    writeIORef ref $ graph {gCB = cb}

graphDrawFitToScreen :: RGraphDraw -> IO ()
graphDrawFitToScreen ref = do
    graph <- readIORef ref
    let gw = gScrolledWindow graph
        (x,y,width,height) = dimensions graph
    (xsize, ysize) <- G.widgetGetSize gw
    --putStrLn $ "Dimensions:" ++ show (x,y, width, height)
    --putStrLn $ "Visible area:" ++ show (xsize,ysize)
    let xscaling = if x+width == 0
                      then 1 
                      else min 1 ((fromIntegral xsize) / (x+width+defNodeRadius+fitMargin))
        yscaling = if y+height == 0
                      then 1 
                      else min 1 ((fromIntegral ysize) / (y+height+defNodeRadius+fitMargin))
        scaling = (max (min xscaling yscaling) minScaling) * 100
    --putStrLn $ "scaling = " ++ show scaling
    G.rangeSetValue (gScale graph) scaling
    changeScale ref scaling
    forceGraphUpdate ref

graphDrawSetMode :: RGraphDraw -> GMode -> IO ()
graphDrawSetMode ref mode = do
    graph <- readIORef ref
    writeIORef ref $ graph {gMode = mode}
    forceGraphUpdate ref

graphDrawInsertNode :: RGraphDraw -> GNodeId -> [GAnnotation] -> (Double, Double) -> Double -> (GC, GC) -> IO ()
graphDrawInsertNode ref id annots coords rscale (linestyle, areastyle) = do
    graph <- readIORef ref
    let newgraph = graph {gGraph = (insNode (id, GNode annots coords rscale linestyle areastyle) (gGraph graph))}
    writeIORef ref newgraph
    forceGraphUpdate ref

graphDrawDeleteNode :: RGraphDraw -> GNodeId -> IO ()
graphDrawDeleteNode ref node = do
    graph <- readIORef ref
    writeIORef ref $ graph {gGraph = delNode node (gGraph graph)}
    forceGraphUpdate ref

graphDrawGetNodeCoords :: RGraphDraw -> GNodeId -> IO (Double, Double)
graphDrawGetNodeCoords ref id = do
    graph <- readIORef ref
    let node = gNode (gGraph graph) id
    return $ gnCoords node

graphDrawSetNodeCoords :: RGraphDraw -> GNodeId -> (Double, Double) -> IO ()
graphDrawSetNodeCoords ref node coords = do
    graph <- readIORef ref
    writeIORef ref $ setNodeCoords graph node coords
    forceGraphUpdate ref

graphDrawGetNodeAnnots :: RGraphDraw -> GNodeId -> IO [GAnnotation]
graphDrawGetNodeAnnots ref id = do
    graph <- readIORef ref
    let node = gNode (gGraph graph) id
    return $ gnAnnot node

graphDrawSetNodeAnnots :: RGraphDraw -> GNodeId -> [GAnnotation] -> IO ()
graphDrawSetNodeAnnots ref node annots = do
    graph <- readIORef ref
    let g' = gmap (\(to, nodeid, n, from) -> 
                    (to, nodeid, n {gnAnnot = (if nodeid == node then annots else gnAnnot n)}, from)) (gGraph graph)
    writeIORef ref $ graph {gGraph = g'}
    forceGraphUpdate ref

graphDrawSetNodeStyle :: RGraphDraw -> GNodeId -> (GC, GC) -> IO ()
graphDrawSetNodeStyle ref id (linestyle, areastyle) = do
    graph <- readIORef ref
    let g' = gmap (\(to, nodeid, n, from) -> 
                    (to, nodeid, if nodeid == id then n {gnLineStyle = linestyle, gnAreaStyle = areastyle} else n, from)) (gGraph graph)
    writeIORef ref $ graph {gGraph = g'}
    forceGraphUpdate ref

graphDrawInsertEdge :: RGraphDraw -> GNodeId -> GNodeId -> GEdgeId -> [GAnnotation] -> GC -> EndStyle -> Bool -> IO ()
graphDrawInsertEdge ref from to id annots style end visible = do
    graph <- readIORef ref 
    let corners = defaultCorners (gGraph graph) from to
        g = insEdge (from, to, GEdge id annots corners style end visible) (gGraph graph) 
    writeIORef ref $ graph {gGraph = g}
    forceGraphUpdate ref


graphDrawDeleteEdge :: RGraphDraw -> GEdgeId -> IO ()
graphDrawDeleteEdge ref id = do
    graph <- readIORef ref
    let edge = gEdge (gGraph graph) id
    writeIORef ref $ graph {gGraph = delLEdge edge (gGraph graph)}
    forceGraphUpdate ref

graphDrawUpdateEdgeEnds :: RGraphDraw -> GEdgeId -> GNodeId -> GNodeId -> IO ()
graphDrawUpdateEdgeEnds ref id from' to' = do
    graph <- readIORef ref
    let (from,to,ge) = gEdge (gGraph graph) id
        corners = defaultCorners (gGraph graph) from' to'
        g'  = delLEdge (from,to,ge) (gGraph graph)
        g'' = insEdge (from',to',ge{geCorners = corners}) g'
    writeIORef ref $ graph {gGraph = g''}
    forceGraphUpdate ref

graphDrawGetEdgeAnnots :: RGraphDraw -> GEdgeId -> IO [GAnnotation]
graphDrawGetEdgeAnnots ref id = do
    graph <- readIORef ref
    let (_,_,ge) = gEdge (gGraph graph) id
    return $ geAnnot ge

graphDrawSetEdgeAnnots :: RGraphDraw -> GEdgeId -> [GAnnotation] -> IO ()
graphDrawSetEdgeAnnots ref id annots = do
    graph <- readIORef ref
    let (from,to,ge) = gEdge (gGraph graph) id
        ge' = ge {geAnnot = annots}
        g'  = delLEdge (from,to,ge) (gGraph graph)
        g'' = insEdge (from,to,ge') g'
    writeIORef ref $ graph {gGraph = g''}
    forceGraphUpdate ref

graphDrawSetEdgeVisible :: RGraphDraw -> GEdgeId -> Bool -> IO ()
graphDrawSetEdgeVisible ref id b = do
    graph <- readIORef ref
    let (from,to,ge) = gEdge (gGraph graph) id
        ge' = ge {geVisible = b}
        g'  = delLEdge (from,to,ge) (gGraph graph)
        g'' = insEdge (from,to,ge') g'
    writeIORef ref $ graph {gGraph = g''}
    forceGraphUpdate ref

graphDrawSetEdgeStyle :: RGraphDraw -> GEdgeId -> GC -> IO ()
graphDrawSetEdgeStyle ref id style = do
    graph <- readIORef ref
    let (from,to,ge) = gEdge (gGraph graph) id
        ge' = ge {geLineStyle = style}
        g'  = delLEdge (from,to,ge) (gGraph graph)
        g'' = insEdge (from,to,ge') g'
    writeIORef ref $ graph {gGraph = g''}
    forceGraphUpdate ref

graphDrawSetEdgeCorners :: RGraphDraw -> GEdgeId -> [(Double, Double)] -> IO ()
graphDrawSetEdgeCorners ref id corners = do
    graph <- readIORef ref
    writeIORef ref $ updateEdgeCorners graph id corners
    forceGraphUpdate ref

graphDrawStraightenEdge :: RGraphDraw -> GEdgeId -> IO ()
graphDrawStraightenEdge ref id = do
    graph <- readIORef ref
    let (from, to, _) = gEdge (gGraph graph) id
        corners = defaultCorners (gGraph graph) from to
    graphDrawSetEdgeCorners ref id corners


graphDrawAutoLayout :: RGraphDraw -> FilePath -> [GEdgeId] -> IO (Either String ())
graphDrawAutoLayout ref fname edges = do
    graph <- readIORef ref
    let g' = autoLayoutPreprocess (gGraph graph) edges
        -- Convert graph into dot format
        --graphstr = graphviz (gmap (\(pred, id, _, suc) -> (pred, id, id, suc)) $ emap geId g') "" 
        --                    (6.0, 11.0) (1,1) Portrait
        -- parse the resulting graph
        --gv = (GVT.parseDotGraph (T.pack graphstr))::(GVTG.DotGraph Int)
        gv = GVTG.mkGraph (map (\n -> GVT.DotNode n []) $ nodes g')
                          (map (\(f,t,e) -> GVT.DotEdge f t []) $ labEdges g')
    -- dump automatic layout into file
    catch (do runGraphvizCommand Data.GraphViz.Commands.Dot gv DotOutput fname
              layoutstr <- readFile fname
              let layoutgr = (GVT.parseDotGraph (T.pack layoutstr))::(GVTG.DotGraph Int)
              dotToTransitionGraph ref (GVTG.toCanonical layoutgr)
              forceGraphUpdate ref
              return $ Right ())
          (\e -> return $ Left (show (e::IOException)))


graphDrawToString :: RGraphDraw -> IO String
graphDrawToString ref = do
    graph <- readIORef ref
    return $ gshow ((labNodes $ gGraph graph), (labEdges $ gGraph graph))


safeRead :: (Data a) => String -> Maybe a
safeRead (gread -> [(v,"")]) = Just v
safeRead _ = Nothing

graphDrawFromString :: RGraphDraw -> String -> IO (Either String ())
graphDrawFromString ref str = do
    graph <- readIORef ref
    case (safeRead str)::(Maybe ([LNode GNode],[LEdge GEdge])) of
         Nothing -> return $ Left "Failed to parse graph layout"
         Just (nodes, edges) -> do let withnodes = foldl' (\g n -> insNode n g) empty nodes
                                       withedges = foldl' (\g e -> insEdge e g) withnodes edges
                                   writeIORef ref $ graph {gGraph = withedges}
                                   forceGraphUpdate ref
                                   return $ Right ()

graphDrawSetGr :: RGraphDraw -> GraphDrawGr -> IO ()
graphDrawSetGr ref gr = do
    graph <- readIORef ref
    writeIORef ref $ graph {gGraph = gr}
    forceGraphUpdate ref

graphDrawGetNodesAtLocation :: RGraphDraw -> (Double, Double) -> IO [GNodeId]
graphDrawGetNodesAtLocation ref (x,y) = do
    graph <- readIORef ref
    return $ nodesAtLocation graph x y

----------------------------------------------------------
-- GUI event handlers
----------------------------------------------------------

mouseMove :: RGraphDraw -> EventM EMotion Bool
mouseMove ref = do
    (x', y') <- G.eventCoordinates
    liftIO $ do 
    	--putStrLn $ "Mouse " ++ (show (x',y'))
	graph <- readIORef ref
        let (x, y) = unscale graph (x', y')
        let corners = cornersAtLocation graph x y
            corner = listToMaybe corners 
            states = nodesAtLocation graph x y
            state = listToMaybe states
            edges = edgesAtLocation graph x y
            actedge = case edges of
                          [] -> Nothing
                          ((_,_,e),_):_ -> Just $ geId e
        let graph' = graph {gMousePosition = (x, y), gActiveCorner = corner, gActiveNode = state, gActiveEdge = actedge}
	writeIORef ref graph'
        if (gActiveCorner graph /= gActiveCorner graph') || (gActiveNode graph /= gActiveNode graph') || (gActiveEdge graph /= gActiveEdge graph')
           then forceGraphUpdate ref
           else return()
        case gMode graph of
	     GLocate _ -> forceGraphUpdate ref
             GNormal   -> case gState graph of
                               GNodeDrag node oldgr -> do let (oldx, oldy) = gnCoords $ gNode oldgr node
                                                              graph'' = moveNode oldgr graph' node (oldx, oldy) (gMousePosition graph')
                                                          writeIORef ref graph''
                                                          forceGraphUpdate ref
                               GCornerDrag (from,to,edge) (Left idx) -> do let corners = (take idx $ geCorners edge) ++ [(x,y)] ++ (drop idx $ geCorners edge)
                                                                               edge' = edge{geCorners = corners}
                                                                               graph' = updateEdgeCorners graph (geId edge) corners
                                                                           writeIORef ref graph'
                                                                           setState ref (GCornerDrag (from,to,edge') (Right idx))
                                                                           forceGraphUpdate ref
                               GCornerDrag (from,to,edge) (Right idx) -> do let corners = (take idx $ geCorners edge) ++ [(gMousePosition graph')] ++ (drop (idx+1) $ geCorners edge)
                                                                                graph'' = updateEdgeCorners graph' (geId edge) corners
                                                                            writeIORef ref graph''
                                                                            forceGraphUpdate ref
                               GIdle -> return ()
    return True


graphMouseClick :: RGraphDraw -> EventM EButton Bool
graphMouseClick ref = do
    button <- G.eventButton
    click <- G.eventClick
    (x',y') <-  G.eventCoordinates
    liftIO $ do
	graph <- readIORef ref
        let (x,y) = unscale graph (x',y')
	let states = nodesAtLocation graph x y
        let corners = cornersAtLocation graph x y
        --putStrLn $ "cornersAtLocation: " ++ (show $ map (\((from, to,_),idx) -> ((from,to),idx)) corners)
	let edges = edgesAtLocation graph x y
        --putStrLn $ "edgesAtLocation: " ++ (show $ map (\((from, to,_),idx) -> ((from,to),idx)) edges)

    	case (gMode graph, gState graph, states, edges, corners, button, click) of
	     -- Right-click on a node
	     (GNormal, GIdle, node:_, _, _, G.RightButton, Event.SingleClick) ->  do 
                 (onNodeRightClick $ gCB graph) node
		 return True
	     -- Right-click on a corner opens a context menu for this corner
	     (GNormal, GIdle, _, _, corner:_, G.RightButton, Event.SingleClick) ->  do 
                 showCornerMenu ref corner
		 return True
	     -- Right-click on an edge opens a context menu for this edge
	     (GNormal, GIdle, _, ((from, to, edge),_):_, [], G.RightButton, Event.SingleClick) ->  do 
                 (onEdgeRightClick $ gCB graph) (from,to,geId edge)
		 return True
             -- Right-click on empty space opens another context menu
	     (GNormal, GIdle, [], _, [], G.RightButton, Event.SingleClick) ->  do 
                 (onEmptyRightClick $ gCB graph) (x,y)
		 return True 
	     -- Left-click on a state starts the state dragging mode 
	     -- and shows the content of the state in the StateInspector panel
	     (GNormal, GIdle, node:_, _, [], G.LeftButton, Event.SingleClick) -> do 
                 setState ref (GNodeDrag node (gGraph graph))
                 (onNodeLeftClick $ gCB graph) node
	         return True
	     (GNormal, GNodeDrag node oldgr, _, _, _, G.LeftButton, Event.ReleaseClick) -> do 
                 let fromcoords = gnCoords (gNode oldgr node)
                     tocoords = gnCoords (gNode (gGraph graph) node)
                 setState ref GIdle
                 if fromcoords == tocoords 
                              then return ()
                              else (onNodeMoved $ gCB graph) node fromcoords tocoords
		 return True
             -- Left-click on an edge creates a new corner and starts the corner dragging mode
	     (GNormal, GIdle, _, ((from,to,edge),idx):_, [], G.LeftButton, Event.SingleClick) -> do 
                 (onEdgeLeftClick $ gCB graph) (from,to,geId edge)
                 setState ref (GCornerDrag (from,to,edge) (Left idx))
                 return True
             -- Left-click on a corner starts a corner dragging mode
	     (GNormal, GIdle, _, _, ((from,to,edge),idx):_, G.LeftButton, Event.SingleClick) -> do 
                 setState ref (GCornerDrag (from,to,edge) (Right idx))
                 return True
             (GNormal, GCornerDrag _ _, _, _, _, G.LeftButton, Event.ReleaseClick) -> do 
                 (onEdgeMoved $ gCB graph) 
                 setState ref GIdle
                 return True
	     -- The user is choosing a location for a new node.  Only
	     -- react if there is no node in this location.  Left-button
	     -- click places the node.  Right-button click cancels the 
	     -- operation.
	     (GLocate _, _, [], _, _, _, Event.SingleClick) -> do
                 (onLocateEmpty $ gCB graph) button (x,y)
                 return True
	     (GLocate _, _, node:_, _, _, _, Event.SingleClick) -> do
                 (onLocateNode $ gCB graph) button node
                 return True
	     -- Ignore anything else
	     _ -> return True



setState :: RGraphDraw -> GState -> IO ()
setState ref state = do
    graph <- readIORef ref
    writeIORef ref $ graph {gState = state}
    forceGraphUpdate ref

showCornerMenu :: RGraphDraw -> ((Int, Int, GEdge),Int) -> IO ()
showCornerMenu ref ((from, to, edge), idx) = do
    menu <- G.menuNew
    dnew <- G.menuItemNewWithLabel "Delete corner"
    G.on dnew G.menuItemActivate (deleteCorner ref ((from, to, edge),idx))
    G.containerAdd menu dnew
    G.widgetShow dnew
    G.menuPopup menu Nothing

deleteCorner :: RGraphDraw -> ((Int, Int, GEdge), Int) -> IO ()
deleteCorner ref ((from, to, edge), idx) = do
    graph <- readIORef ref
    corners <- if (from == to) && ((length $ geCorners edge) == 1)
                  then return $ geCorners edge
                  else return $ (take idx $ geCorners edge) ++ (drop (idx+1) $ geCorners edge)
    let graph' = updateEdgeCorners graph (geId edge) corners
    let graph'' = if Just ((from, to, edge), idx) == gActiveCorner graph' 
                     then graph'{gActiveCorner = Nothing}
                     else graph'
    writeIORef ref graph''
    forceGraphUpdate ref



----------------------------------------------------------
-- Graph coordinates computation
----------------------------------------------------------

nodesAtLocation :: GraphDraw -> Double -> Double -> [GNodeId]
nodesAtLocation graph x y =
    let nodeids = nodes $ gGraph graph
    in filter (\node -> isNodeAtLocation graph node x y) nodeids

nodeCoords :: GraphDraw -> Node -> (Double, Double)
nodeCoords graph node = gnCoords $ gNode (gGraph graph) node

nodeRadius :: GraphDraw -> Node -> Double
nodeRadius graph node = defNodeRadius * (gnRScale $ gNode (gGraph graph) node)

edgesAtLocation :: GraphDraw -> Double -> Double -> [((GNodeId,GNodeId,GEdge), Int)]
edgesAtLocation graph x y =
    foldr (\(from, to, edge) es -> let (dist,segment) = distanceToEdge graph x y (from, to, edge)
                                   in if dist < edgeProximityThreshold
                                         then ((from,to, edge),segment):es
                                         else es) [] (labEdges $ gGraph graph)

cornersAtLocation :: GraphDraw -> Double -> Double -> [((GNodeId,GNodeId,GEdge),Int)]
cornersAtLocation graph x y = 
    foldr (\(from, to, edge) es -> case edgeCornerAtLocation x y edge of
                                        Nothing -> es
                                        Just idx -> ((from,to,edge),idx):es) [] (labEdges $ gGraph graph)

edgeCornerAtLocation :: Double -> Double -> GEdge -> Maybe Int
edgeCornerAtLocation x y edge = 
    fmap (fst) (find (\(idx, (cx, cy)) -> (distance (x,y) (cx,cy)) < cornerProximityThreshold ) (zip [0..] (geCorners edge)))

distance :: (Double,Double) -> (Double,Double) -> Double
distance (x1,y1) (x2,y2) = 
    ((x2-x1) * (x2-x1) + (y2-y1) * (y2-y1)) ** 0.5

distanceToEdge :: GraphDraw -> Double -> Double -> (GNodeId,GNodeId,GEdge) -> (Double, Int)
distanceToEdge graph x y (from, to, edge) = 
    let path = edgeToPath graph from to (geCorners edge)
    in foldr (\(idx, line) (curmin,curidx) -> let dist = (distanceToLine x y line)
                                              in if min curmin dist == curmin
                                                    then (curmin,curidx)
                                                    else (dist,idx)) ((distanceToLine x y (head path)),0) (zip [1..] (tail path))

distanceToLine :: Double -> Double -> ((Double, Double),(Double, Double)) -> Double
distanceToLine x y ((x',y'),(x'',y'')) = 
    let b = ((x'-x)*(x'-x) + (y'-y) * (y'-y))**0.5
        c = ((x''-x)*(x''-x) + (y''-y) * (y''-y))**0.5
        a = ((x'-x'')*(x'-x'') + (y'-y'') * (y'-y''))**0.5
        p = (a+b+c)/2
    in if a > 2.0
          then let h = 2 * (((p*(p-a)*(p-b)*(p-c)))**0.5)/a
                   cosa = (a*a + c*c - b*b)/(2*a*c)
                   cosb = (b*b + a*a - c*c)/(2*b*a)
               in if cosa < 0 || cosb < 0
                     then min b c
                     else h
          else min b c
    
--{-trace ("distance: " ++ show distance ++ ", b=" ++ show b ++ ", c=" ++ show c ++ ", h=" ++ show h)-} distance


edgeToPath :: GraphDraw -> GNodeId -> GNodeId -> [(Double, Double)] -> [((Double, Double),(Double, Double))]
edgeToPath graph from to corners = 
    let (x1, y1) = nodeCoords graph from
        (x2, y2) = nodeCoords graph to
        fradius = nodeRadius graph from
        tradius = nodeRadius graph to
        points = (x1,y1):corners ++ [(x2,y2)]
        rpoints = {-trace ("points:" ++ show points)-} reverse points
        -- the edge must start and end at the boundary of the state circle, not in the center
        firstSegmentLen = (((fst $ head $ tail points)-x1) * ((fst $ head $ tail points)-x1) + ((snd $ head $ tail points)-y1) * ((snd $ head $tail points)-y1)) ** 0.5
        lastSegmentLen = ((x2 - (fst $ head $ tail rpoints)) * (x2 - (fst $ head $ tail rpoints)) + (y2 - (snd $ head $ tail rpoints)) * (y2 - (snd $ head $ tail rpoints))) ** 0.5
        x1' = if firstSegmentLen == 0
                then x1
                else x1 + ((fradius * ((fst $ head $ tail points)-x1)) / firstSegmentLen)
        y1' = if firstSegmentLen == 0
                 then y1
                 else y1 + ((fradius * ((snd $ head $ tail points) - y1)) / firstSegmentLen)
        x2' = if lastSegmentLen == 0
                 then x2
                 else x2 + ((tradius * ((fst $ head $ tail rpoints) - x2)) / lastSegmentLen)
        y2' = if lastSegmentLen == 0
                 then y2
                 else y2 + ((tradius * ((snd $ head $ tail rpoints) - y2)) / lastSegmentLen)
        points' = (x1',y1'):corners ++ [(x2',y2')]
    in {-trace ("points': " ++ (show $ init $ init $ tails points'))-}foldr (\ps pairs -> (head ps, head $ tail ps):pairs) [] (init $ init $ tails points')



isNodeAtLocation :: GraphDraw -> Node -> Double -> Double -> Bool
isNodeAtLocation graph node x y = 
    -- TODO: use different radius for simple states and sets of states
    let (x',y') = nodeCoords graph node
        r = nodeRadius graph node
    in (x-x') * (x-x') + (y-y') * (y-y') <= r * r

lineSlope :: Double -> Double -> Double -> Double -> Double
lineSlope x y x' y' = 
    let shift = if (x'-x >= 0) then 0
			       else pi::Double
    in if x' == x
          then if y'-y > 0 
                  then shift + pi/2
                  else shift - pi/2
          else shift + atan ((y' - y) / (x' - x))

moveNode :: GraphDrawGr -> GraphDraw -> GNodeId -> (Double, Double) -> (Double, Double) -> GraphDraw
moveNode oldgr graph nodeid (x, y) (x', y') =
    let suc = lsuc oldgr nodeid
        pre = lpre oldgr nodeid
        graph' = foldr (\(to, edge) g -> let ((x2,y2),(x2',y2')) = if to == nodeid
                                                                      then ((x,y), (x',y'))
                                                                      else (gnCoords $ gNode oldgr to, gnCoords $ gNode oldgr to)
                                             corners' = map (moveCorner (x,y) (x',y') (x2,y2) (x2',y2')) (geCorners edge)
                                         in updateEdgeCorners g (geId edge) corners') 
                       graph suc
        graph'' = foldr (\(from, edge) g -> let ((x2,y2),(x2',y2'))= if from == nodeid
                                                                        then ((x,y),(x',y'))
                                                                        else (gnCoords $ gNode oldgr from, gnCoords $ gNode oldgr from)
                                                corners' = map (moveCorner (x2,y2) (x2',y2') (x,y) (x',y')) (geCorners edge)
                                            in updateEdgeCorners g (geId edge) corners') graph' pre
        graph''' = setNodeCoords graph'' nodeid (x', y')
    in graph'''

moveCorner :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
moveCorner (x1,y1) (x1',y1') (x2,y2) (x2',y2') (x,y) = 
    let a = ((x2-x1) * (x2-x1) + (y2-y1) * (y2-y1)) ** 0.5
        b = ((x-x2) * (x-x2) + (y-y2) * (y-y2)) ** 0.5
        c = ((x-x1) * (x-x1) + (y-y1) * (y-y1)) ** 0.5
        a' = ((x2'-x1') * (x2'-x1') + (y2'-y1') * (y2'-y1')) ** 0.5
    in {-trace ("moveCorner " ++ show (x1,y1) ++ show (x1',y1') ++ show (x2, y2) ++ show (x2',y2') ++ show (x,y)) $-}
        if a < 1 
           then (x + (x1' - x1), y + (y1' - y1))
           else if c < 1
                then (x1', y1')
                else let c' = c * a' /a
                         alpha = (atan2 (y-y1) (x-x1)) - (atan2 (y2-y1) (x2-x1) )
                         beta = atan2 (y2'- y1') (x2'-x1')
                     in {-trace ("c'=" ++ show c')-} (x1' + c' * (cos (alpha+beta)), y1' + c' * (sin (alpha + beta)))


-- Update the list of corners of a graph edge
updateEdgeCorners :: GraphDraw -> GEdgeId -> [(Double,Double)] -> GraphDraw
updateEdgeCorners graph id corners =
    let g = gGraph graph
        (from,to,tge) = gEdge g id
        g' = delLEdge (from,to,tge) g
        g'' = insEdge (from,to,tge{geCorners = corners}) g'
    in graph {gGraph = g''}


setNodeCoords :: GraphDraw -> GNodeId -> (Double, Double) -> GraphDraw
setNodeCoords graph node coords = graph {gGraph = g'}
    where g' = gmap (\(to, nodeid, n, from) -> 
                    (to, nodeid, n{gnCoords = (if nodeid == node then coords else gnCoords n)}, from)) (gGraph graph)

loopTransitionCorners :: GNode -> [(Double, Double)]
loopTransitionCorners node = 
    let (x,y) = (fst $ gnCoords node, snd $ gnCoords node)
    in [(x+loopWidth,y-loopHeight), (x+loopWidth, y+loopHeight)]

defaultCorners g from to = 
    if (from /= to)
       then []
       else loopTransitionCorners (fromJust $ lab g from)

--------------------------------------------
---- Graph drawing
--------------------------------------------

forceGraphUpdate :: RGraphDraw -> IO ()
forceGraphUpdate ref = do
    graph <- readIORef ref
    if gConnected graph
       then do --putStrLn "forceGraphUpdate"
               updatePixmap ref
               graph <- readIORef ref
               --print $ show $ gdTransGr dbg
               drawWin <- G.widgetGetDrawWindow $ gDrawingArea graph
               (x,y) <- G.drawableGetSize drawWin
               G.drawWindowInvalidateRect drawWin (G.Rectangle 0 0 x y) True	
       else return ()

updateGraph :: RGraphDraw -> G.EventM G.EExpose Bool
updateGraph ref = do
    win <- G.eventWindow
    region <- G.eventRegion 

    liftIO $ do
        updatePixmap ref
        G.Rectangle regx regy regw regh <- G.regionGetClipbox region
	--putStrLn $ "Redraw" ++ show (Rectangle regx regy regw regh)
	graph <- readIORef ref
	let pm = fromJust $ gPixmap graph
	gc <- G.gcNew win
	G.drawDrawable win gc pm regx regy regx regy regw regh
	updateSizeRequest ref (G.Rectangle regx regy regw regh)
	ptr <- G.drawWindowGetPointer win
	return ()
    return True

updateSizeRequest :: RGraphDraw -> G.Rectangle -> IO ()
updateSizeRequest ref (G.Rectangle x y w h) = do
    graph <- readIORef ref
    let area = gDrawingArea graph
    (width,height) <- G.widgetGetSize area
    let newWidth = if width < x+w+20 then min (x+w+20) pixmapWidth
				     else width
    let newHeight = if height < y+h+20 then min (y+h+20) pixmapHeight
				     else height
    --putStrLn $ "size: " ++ (show newWidth) ++ "x" ++ (show newHeight)
    if (newHeight /= height) || (newWidth /= width) then do G.widgetSetSizeRequest area newWidth newHeight
						    	    forceGraphUpdate ref
					    	    else return ()

updatePixmap :: RGraphDraw -> IO ()
updatePixmap ref = do
    --putStrLn "update pixmap"
    graph <- readIORef ref

    let gw = gScrolledWindow graph
    -- Create empty pixmap the size of the draw window
    dw <- G.widgetGetDrawWindow $ gDrawingArea graph
    pm <- case gPixmap graph of
               Nothing -> G.pixmapNew (Just dw) pixmapWidth pixmapHeight Nothing
               Just pixmap -> return pixmap
    gc <- G.gcNewWithValues pm (gcToGCV backgroundStyle)

    hsb <- G.scrolledWindowGetHScrollbar gw
    xcoord <- case hsb of
                   Nothing -> return 0
                   Just sb -> G.rangeGetValue sb
    vsb <- G.scrolledWindowGetVScrollbar gw
    ycoord <- case vsb of
                   Nothing -> return 0
                   Just sb -> G.rangeGetValue sb
    (xsize, ysize) <- G.widgetGetSize gw

--    putStrLn $ "Visible area: " ++ (show $ fromIntegral $ round xcoord) ++ ", " ++ 
--                                   (show $ fromIntegral $ round ycoord) ++ ", " ++
--                                   (show xsize) ++ ", " ++ (show ysize)
    G.drawRectangle pm gc True (fromIntegral $ round xcoord) (fromIntegral $ round ycoord) xsize ysize
    -- Draw the transition graph on the pixmap
    let graph' = graph {gPixmap = Just pm}
    fillPixmap graph'
    writeIORef ref graph'

fillPixmap :: GraphDraw -> IO ()
fillPixmap graph = do
    let pm = fromJust $ gPixmap graph
    mapM  (\node -> drawState graph pm node (nodeCoords graph node)) (nodes $ gGraph graph)
--    let edges = foldr (\(from, to, TGEdge etype _ label trels _) m -> Map.insert (from,to) ((label,trels,etype):(Map.findWithDefault [] (from,to) m)) m) (Map.empty) (labEdges graph)
    mapM (\(from, to, edge) -> drawEdge graph pm from to edge) (labEdges $ gGraph graph)
    case gMode graph of
         GLocate CursorGhost  -> drawGhostState graph pm
         GLocate CursorTarget -> drawTarget graph pm
	 _                    -> return ()
    case gActiveCorner graph of
         Nothing -> return ()
         Just ((_, _, edge), idx) -> drawActiveCorner graph pm ((geCorners edge) !! idx)


drawState :: GraphDraw -> G.Pixmap -> GNodeId -> (Double, Double) -> IO () 
drawState graph pixmap node (x,y) = do
    --putStrLn $ "drawState " ++ (show node)
    let gn = gNode (gGraph graph) node
    let radius = nodeRadius graph node
    gcArea <- (G.gcNewWithValues pixmap) $ gcToGCV $ gnAreaStyle gn
    drawArcD graph pixmap gcArea True (x-radius) (y-radius) (radius * 2) (radius * 2) 0 (64*360)
    gc <- if Just node == gActiveNode graph
             then (G.gcNewWithValues pixmap) $ (gcToGCV $ gnLineStyle gn) {G.lineWidth = (gcLW $ gnLineStyle gn) + 1}
             else (G.gcNewWithValues pixmap) $ gcToGCV $ gnLineStyle gn
    drawArcD graph pixmap gc False (x-radius) (y-radius) (radius * 2) (radius * 2) 0 (64*360)

    let annots = filter ((/= "") . annotText) $ gnAnnot gn
    mapM (\(a,n) -> do layout <- G.widgetCreateLayout (gDrawingArea graph) (annotText a)
                       gct <- (G.gcNewWithValues pixmap) (gcToGCV $ annotTextStyle a)
                       (_, G.Rectangle _ _ _ height) <- G.layoutGetPixelExtents layout
                       let height' = (fromIntegral height) / (gScaling graph)
                       drawLayoutD graph pixmap gct (x + radius + 2) 
                                                    (y + (fromIntegral n*height') - (fromIntegral (length annots) * height') / 2) layout)
         $ zip annots [0..]
    return ()

drawGhostState :: GraphDraw -> G.Pixmap -> IO ()
drawGhostState graph pixmap = do
    --putStrLn $ "drawGhostState"
    gc <- (G.gcNewWithValues pixmap) (gcToGCV ghostStyle)
    let (x,y) = gMousePosition graph
    drawArcD  graph pixmap gc False (x-defNodeRadius) (y-defNodeRadius) (defNodeRadius * 2) (defNodeRadius * 2) 0 (64*360)
    drawLineD graph pixmap gc (x, y-defNodeRadius+1) (x, y-defNodeRadius+8)
    drawLineD graph pixmap gc (x, y+defNodeRadius-1) (x, y+defNodeRadius-8)
    drawLineD graph pixmap gc (x-defNodeRadius+1, y) (x-defNodeRadius+8, y)
    drawLineD graph pixmap gc (x+defNodeRadius-1, y) (x+defNodeRadius-8, y)

drawTarget :: GraphDraw -> G.Pixmap -> IO ()
drawTarget graph pixmap = do
    --putStrLn $ "drawGhostState"
    gc <- (G.gcNewWithValues pixmap) (gcToGCV targetStyle)
    let (x,y) = (gMousePosition graph)
        --x' = (fromIntegral $ round $ x) :: Int
        --y' = (fromIntegral $ round $ y) :: Int
    drawLineD graph pixmap gc (x-targetRadius, y) (x+targetRadius, y)
    drawLineD graph pixmap gc (x, y-targetRadius) (x, y+targetRadius)


drawActiveCorner :: GraphDraw -> G.Pixmap -> (Double, Double) -> IO ()
drawActiveCorner graph pixmap (x,y) = do
    gc <- (G.gcNewWithValues pixmap) (gcToGCV activeCornerStyle)
    drawArcD graph pixmap gc True (x-activeCornerRadius) (y-activeCornerRadius) (activeCornerRadius * 2) (activeCornerRadius * 2) 0 (64*360)


drawEdge :: GraphDraw -> G.Pixmap -> GNodeId -> GNodeId -> GEdge -> IO ()
drawEdge graph pixmap from to edge = do
    if geVisible edge
       then drawEdge' graph pixmap from to edge
       else return ()

drawEdge' :: GraphDraw -> G.Pixmap -> GNodeId -> GNodeId -> GEdge -> IO ()
drawEdge' graph pixmap from to edge = do
    let area = gDrawingArea graph
        path = edgeToPath graph from to (geCorners edge)

    --putStrLn $ "drawStraightEdge " ++ (show from) ++ "->" ++ (show to)
    --putStrLn $ "Path: " ++ show path
    let edgeStyle = if Just (geId edge) == gActiveEdge graph  
                       then (gcToGCV $ geLineStyle edge) {G.lineWidth = (gcLW $ geLineStyle edge) + 1}
                       else gcToGCV $ geLineStyle edge
    gca <- (G.gcNewWithValues pixmap) edgeStyle

    mapM (\((x,y),(x',y')) -> drawLineD graph pixmap gca (x,y) (x',y')) path

    -- compute the slope of the edge and draw the arrow under the right angle
    let ((lastx, lasty),(lastx', lasty')) = last path
        ((firstx, firsty),(firstx', firsty')) = head path
    let endslope = lineSlope lastx lasty lastx' lasty'
        startslope = lineSlope firstx' firsty' firstx firsty
    case geEndStyle edge of
         EndNone    -> return ()
         EndArrow   -> drawArrow graph pixmap gca endslope (lastx', lasty')
         EndDiamond -> drawDiamond graph pixmap gca endslope (lastx', lasty')
    -- draw text label
    let annots = filter ((/= "") . annotText) $ geAnnot edge
        (lannots, rannots) = partition ((==AnnotLeft) . annotLoc) annots

    rlayouts <- mapM (\a -> G.widgetCreateLayout area (annotText a)) rannots
    llayouts <- mapM (\a -> G.widgetCreateLayout area (annotText a)) lannots

    rrects <- mapM G.layoutGetPixelExtents rlayouts
    lrects <- mapM G.layoutGetPixelExtents llayouts

    let rwidth  = last $ sort $ map (\(_, G.Rectangle _ _ w _) -> w) rrects
        rheight = sum $ map (\(_, G.Rectangle _ _ _ h) -> h) rrects
        lwidth  = last $ sort $ map (\(_, G.Rectangle _ _ w _) -> w) lrects
        lheight = sum $ map (\(_, G.Rectangle _ _ _ h) -> h) lrects
    let ((x1,y1),(x2,y2)) = case ((length path) `mod` 2) of
                                0 -> (\a->(a,a)) $ snd $ path !! (((length path) `div` 2) - 1)
                                1 -> path !! ((length path) `div` 2)
        lwidth' = (fromIntegral lwidth) / (gScaling graph)
        lheight' = (fromIntegral lheight) / (gScaling graph)
        rheight' = (fromIntegral rheight) / (gScaling graph)
        roffset = 3
        loffset = (-lwidth' - 3)
        startx = (x1 + x2) / 2
        starty = (y1 + y2) / 2

    mapM (\((a,l),n) -> do gct <- G.gcNewWithValues pixmap (gcToGCV $ annotTextStyle a)
                           drawLayoutD graph pixmap gct (startx + roffset) (starty + ((fromIntegral n)/(fromIntegral $ length rannots) - 0.5) * rheight') l) 
         $ zip (zip rannots rlayouts) [0..]

    mapM (\((a,l),n) -> do gct <- G.gcNewWithValues pixmap (gcToGCV $ annotTextStyle a)
                           drawLayoutD graph pixmap gct (startx + loffset) (starty + ((fromIntegral n)/(fromIntegral $ length lannots) - 0.5) * lheight') l) 
         $ zip (zip lannots llayouts) [0..]
    return ()

scale :: GraphDraw -> (Double, Double) -> (Double, Double)
scale graph (x,y) = (x * (gScaling graph), y * (gScaling graph))

unscale :: GraphDraw -> (Double, Double) -> (Double, Double)
unscale graph (x,y) = (x / (gScaling graph), y / (gScaling graph))

scale' :: GraphDraw -> (Double, Double) -> (Int,Int)
scale' graph (x,y) = (fromIntegral $ round $ x * (gScaling graph), fromIntegral $ round $ y * (gScaling graph))

changeScale :: RGraphDraw -> Double -> IO ()
changeScale ref scale = do
     graph <- readIORef ref
     let graph' = graph{gScaling=(scale/100)}
     let area = gDrawingArea graph'
     pango <- G.widgetGetPangoContext area
     font  <- G.contextGetFontDescription pango
     fsize <- G.fontDescriptionGetSize font
     G.fontDescriptionSetSize font (defaultFontSize * (gScaling graph'))
     G.contextSetFontDescription pango font
     writeIORef ref graph'

-- Compute graph dimensions.  Returns coordinates of the top left corner, 
-- width and height of the graph determined by its extreme points
dimensions :: GraphDraw -> (Double, Double, Double, Double)
dimensions graph =
    case labNodes $ gGraph graph of 
        [] -> (0, 0, 0, 0) 
        (_, node):rest -> 
            let (x0,y0) = gnCoords node
                (minx, maxx, miny, maxy) = 
                  foldr (\(_, GNode _ (x,y) _ _ _) (minx, maxx, miny, maxy) -> 
                          ((min x minx), (max x maxx), (min y miny), (max y maxy))) (x0,x0,y0,y0) rest
            in (minx, miny, (maxx-minx), (maxy-miny))


drawLineD :: GraphDraw -> G.Pixmap -> G.GC -> (Double, Double) -> (Double, Double) -> IO ()
drawLineD graph d gc coords1 coords2 = do
    --putStrLn $ "drawLineD " ++ show (x1,y1) ++ show (x2,y2)
    let (x1,y1) = scale' graph coords1
        (x2,y2) = scale' graph coords2
    G.drawLine d gc (x1,y1) (x2, y2)

drawArcD :: GraphDraw -> G.Pixmap -> G.GC -> Bool -> Double -> Double -> Double -> Double -> Int -> Int -> IO ()
drawArcD graph d gc filled x y width height astart aend = do
    let (x',y') = scale' graph (x, y)
        (width',height') = scale' graph (width, height)
    G.drawArc d gc filled x' y' width' height' astart aend

drawLayoutD :: GraphDraw -> G.Pixmap -> G.GC -> Double -> Double -> G.PangoLayout -> IO ()
drawLayoutD graph d gc x y layout = do
    let (x',y') = scale' graph (x, y)
    G.drawLayout d gc x' y' layout

drawArrow :: GraphDraw -> G.Pixmap -> G.GC -> Double -> (Double, Double) -> IO ()
drawArrow graph d gc slope (x2, y2) = do
    let angle1 = (slope + arrowAngle)::Double
    let angle2 = (slope - arrowAngle)::Double
    let ax1 = x2 - arrowLen * (cos angle1)
    let ay1 = y2 - arrowLen * (sin angle1)
    let ax2 = x2 - arrowLen * (cos angle2)
    let ay2 = y2 - arrowLen * (sin angle2)
    drawLineD graph d gc (x2, y2) (ax1, ay1)
    drawLineD graph d gc (x2, y2) (ax2, ay2)


drawDiamond :: GraphDraw -> G.Pixmap -> G.GC -> Double -> (Double, Double) -> IO ()
drawDiamond graph d gc slope (x2, y2) = do
    let angle1 = (slope + arrowAngle)::Double
    let angle2 = (slope - arrowAngle)::Double
    let ax1 = x2 - arrowLen * (cos angle1)
    let ay1 = y2 - arrowLen * (sin angle1)
    let ax2 = x2 - arrowLen * (cos angle2)
    let ay2 = y2 - arrowLen * (sin angle2)
    let ax3 = x2 - arrowLen * (cos angle1) - arrowLen * (cos angle2)
    let ay3 = y2 - arrowLen * (sin angle1) - arrowLen * (sin angle2)
    drawLineD graph d gc (x2, y2) (ax1, ay1)
    drawLineD graph d gc (x2, y2) (ax2, ay2)
    drawLineD graph d gc (ax3, ay3) (ax1, ay1)
    drawLineD graph d gc (ax3, ay3) (ax2, ay2)


--------------------------------------------
---- Conversion to/from dot format
--------------------------------------------

autoLayoutPreprocess :: GraphDrawGr -> [GEdgeId] -> GraphDrawGr
autoLayoutPreprocess g edges = 
    let alledges = labEdges g
    in foldr (\(from, to, ge) tg -> if elem (geId ge) edges 
                                       then tg
                                       else delLEdge (from, to, ge) tg) g alledges

dotNodeId (GVT.DotNode _ attrs) = 
    let (Label (StrLabel lab)) = fromJust $ find (\attr -> case attr of
                                                            Label (StrLabel _) -> True
                                                            _ -> False ) attrs
    in read (T.unpack lab)

dotEdgeId (GVT.DotEdge _ _ attrs) = 
    let mattrs = find (\attr -> case attr of
                                  Label (StrLabel _) -> True
                                  _ -> False) attrs
        (Label (StrLabel lab)) = fromJust $ mattrs
    in read (T.unpack lab)

dotEdgeCorners (GVT.DotEdge _ _ attrs) = 
    let (Pos (SplinePos splines)) = fromJust $ find (\attr -> case attr of
                                                                (Pos (SplinePos _)) -> True
                                                                _ -> False) attrs
        points = foldr (\(Spline _ _ ps) pts -> ps ++ pts) [] splines

        points' = if length points >= 4
                     then reverse $ tail $ tail $ reverse $ tail $ tail points
                     else if length points >= 2
                             then reverse $ tail $ reverse $ tail points
                             else points
    in map (\(Point x y _ _) -> (x, y)) points'


dotNodeCoords (GVT.DotNode _ attrs) = 
    let (Pos (PointPos (Point x y _ _))) = fromJust $ find (\attr -> case attr of
                                                            (Pos (PointPos _)) -> True
                                                            _ -> False ) attrs
    in (fromIntegral $ round $ x, fromIntegral $ round $ y)

dotGraphDimensions gr = 
    let attrs = concat $ map GVT.attrs (GVTC.attrStmts $ GVTC.graphStatements gr)
        BoundingBox (Rect (Point x0 y0 _ _) (Point x1 y1 _ _)) = fromJust $ find (\attr -> case attr of
                                                                                                BoundingBox _ -> True
                                                                                                _ -> False ) attrs
    in (x1, y1)


dotToTransitionGraph :: RGraphDraw -> GVTC.DotGraph Int -> IO ()
dotToTransitionGraph ref layoutgr = do
    let nodes = GVT.graphNodes layoutgr
        edges = GVT.graphEdges layoutgr
        (width, height) = dotGraphDimensions layoutgr
    mapM (\node -> do let id = dotNodeId node
                          (x,y) = dotNodeCoords node
                      graphDrawSetNodeCoords ref id (x, height - y)) nodes
    mapM (\edge -> do let id = dotEdgeId edge
                          corners = dotEdgeCorners edge
                          corners' = map (\(x,y) -> (x, height-y)) corners
                      graphDrawSetEdgeCorners ref id corners') edges
    return ()
