import System.IO
    ( IOMode(ReadMode, WriteMode),
      hPutStrLn,
      hGetContents,
      openFile,
      hClose )
import System.IO.Unsafe
import qualified Graphics.UI.Gtk.Misc.DrawingArea as DRAW
import Graphics.UI.Gtk.Misc.DrawingArea
    (DrawingArea,
      castToDrawingArea,
      drawingAreaNew )
import Graphics.UI.Gtk.General.Enums (TextWindowType(TextWindowLeft))
import Graphics.UI.Gtk
    ( AttrOp((:=)),
      Window,
      Color(Color),
      VBox,
      TreeView,
      TreeModel,
      ToolButton,
      Notebook,
      MenuBar,
      Label,
      ImageMenuItem,
      HBox,
      FileChooserDialog,
      Entry,
      ComboBox,
      Button,
      AboutDialog,
      FileChooserAction(FileChooserActionSave),
      ListStore,
      Packing(PackGrow, PackNatural),
      ConnectId,
      TextView,
      windowResize,
      windowPresent,
      castToWindow,
      castToVBox,
      castToToolButton,
      castToNotebook,
      castToMenuBar,
      castToLabel,
      castToImageMenuItem,
      castToHBox,
      castToFileChooserDialog,
      castToEntry,
      castToComboBox,
      castToCellRendererText,
      castToButton,
      castToAboutDialog,
      fileChooserSetAction,
      fileChooserGetFilename,
      textViewGetBuffer,
      textBufferSetText,
      textBufferSetModified,
      textBufferGetText,
      textBufferGetStartIter,
      textBufferGetEndIter,
      treeViewGetColumns,
      treeViewColumnSetMaxWidth,
      treeViewColumnSetTitle,
      treeViewColumnNew,
      treeViewGetModel,
      treeViewSetModel,
      treeViewNew,
      treeViewAppendColumn,
      listStoreNew,
      listStoreClear,
      listStoreAppend,
      cellTextWidthChars,
      cellTextForegroundColor,
      cellText,
      cellRendererTextNew,
      cellLayoutSetAttributes,
      cellLayoutPackStart,
      onToolButtonClicked,
      menuItemActivated,
      comboBoxSetModelText,
      comboBoxNewText,
      comboBoxGetActive,
      comboBoxSetActive,
      comboBoxAppendText,
      changed,
      hBoxNew,
      mainQuit,
      mainGUI,
      initGUI,
      entrySetText,
      entryNew,
      entryGetText,
      labelSetText,
      labelSetLabel,
      labelNew,
      labelGetText,
      toggleButtonActive,
      toggled,
      linkButtonNewWithLabel,
      checkButtonNewWithLabel,
      buttonNewWithLabel,
      buttonActivated,
      builderNew,
      builderGetObject,
      builderAddFromFile,
      widgetWidthRequest,
      widgetShowAll,
      widgetGetSizeRequest,
      widgetSetSizeRequest,
      widgetSetSensitive,
      widgetHide,
      widgetHeightRequest,
      containerRemove,
      containerGetChildren,
      boxReorderChild,
      boxQueryChildPacking,
      boxPackStart,
      boxPackEnd,
      boxChildPosition,
      boxChildPacking,
      on,
      set,
      windowTitle,
      deleteEvent,
      exposeEvent,
      objectDestroy,
      mapSignal)
import Graphics.UI.Gtk ()
import Graphics.UI.Gtk.Builder ()
import Graphics.UI.Gtk.ModelView as Model
    ( listStoreSetValue,
      castToTreeView,
      castToTreeModel,
      treeViewColumnSetTitle,
      treeViewColumnNew,
      treeViewSetHeadersVisible,
      treeViewNewWithModel,
      treeViewAppendColumn,
      cellText,
      cellRendererTextNew,
      cellLayoutSetAttributes,
      cellLayoutPackStart )
import Graphics.UI.Gtk.SourceView
    ( SourceView,
      sourceViewNewWithBuffer,
      sourceViewNew,
      sourceViewGetGutter,
      sourceLanguageManagerSetSearchPath,
      sourceLanguageManagerNew,
      sourceLanguageManagerGetSearchPath,
      sourceLanguageManagerGetLanguage,
      sourceBufferSetHighlightSyntax,
      sourceBufferNewWithLanguage )
import Graphics.UI.Gtk.Layout.Table ()

import Numeric.LinearAlgebra ( (!) )
import Graphics.UI.Gtk.Gdk.EventM ()
import Graphics.UI.Gtk.MenuComboToolbar.ComboBox ()
import Data.List ( transpose, isInfixOf, delete )
import Data.List.Split ( splitOn )
import CPi.Lib
    ( Species,
      Process,
      Pretty(pretty),
      Name,
      Definition(..),
      speciesInProc,
      prettys )
import CPi.Parser ( parseFormula, parseFile )
import CPi.Semantics ( processMTS )
import CPi.ODE ( timePoints, speciesIn, solveODE, prettyODE, dPdt )
import CPi.Plot
    ( plotTimeSeriesToFile,
      plotTimeSeriesFilteredD,
      plotTimeSeriesToFileFiltered,
      plotTimeSeriesD,
      phasePlot2ToFile,
      phasePlot2D )
import CPi.Logic ( reconcileSpecs )
import CPi.Matlab ()
import CPi.Signals ( modelCheckSig )
import System.GIO.File.AppInfo
    ( appInfoLaunchUris, appInfoGetAllForType )
import qualified Data.List as L ( (\\) )
import qualified Numeric.LinearAlgebra as LA ( Vector, Matrix, toColumns, toList )
import Data.String.Utils as Utils ( strip, replace )
import Data.Tuple ()
import Graphics.UI.Gtk as GTK ( get )
import Hledger.Cli.Utils ()
import qualified Data.Text as T ( singleton, pack, count, unpack )
import Graphics.Rendering.Cairo(
	liftIO)
import System.Glib.Attributes
import System.Glib
-- import System.Glib.UTFString

a @@> (i, j) = a ! i ! j

main :: IO ()
main =
    do
	initGUI
        -- <- newVar []
	--plots <- newVar[]
        gui <- loadGlade "cpi-ide.glade"
	editor gui
	mainGUI


editor :: GUI ->   IO()
editor gui  = do

		lm <- sourceLanguageManagerNew
		sourceLanguageManagerSetSearchPath lm (Just ["./lang/"])
  		langM <- sourceLanguageManagerGetLanguage lm "cpi"
		lang <- case langM of
    			(Just lang) -> return lang
    			Nothing -> do
				langDirs <- sourceLanguageManagerGetSearchPath lm
      				error ("please copy Cpi.lang to one of the following directories:\n" ++unlines langDirs)

  		buffer <- sourceBufferNewWithLanguage lang
  		textBufferSetModified buffer False
  		sourceBufferSetHighlightSyntax buffer True
		mainText <- sourceViewNewWithBuffer buffer
		gutter <- sourceViewGetGutter mainText TextWindowLeft
 		cell   <- cellRendererTextNew
  		--sourceGutterInsert gutter cell 0
		{--sourceGutterSetCellDataFunc gutter cell $ \ c l currentLine -> do
			set (castToCellRendererText c) [cellText := show (l + 1)]
			let color = if currentLine
		                	then Color 65535 0 0
		                    	else Color 0 65535 0
		 	set (castToCellRendererText c) [cellTextForegroundColor := color]
	  	sourceGutterSetCellSizeFunc gutter cell $ \ c ->
      			set (castToCellRendererText c) [cellTextWidthChars := (2)]--}
		widgetSetSizeRequest mainText 450 200

		boxPackStart (hbox1 gui) mainText  PackNatural 0
		boxReorderChild (hbox1 gui) mainText   0
		connectGui gui  mainText
		widgetShowAll mainText

loadGlade :: FilePath -> IO GUI
loadGlade gladepath =
    do
        builder <- builderNew
    	builderAddFromFile builder gladepath
        mw <- builderGetObject builder castToWindow "MainWindow"
	set mw [ windowTitle :=  "CPIDE"]

        windowResize mw 900 400
	mainMenu <- builderGetObject builder castToMenuBar "mainMenu"
	[fileOpen,fileSaveAs,fileQuit] <- mapM (builderGetObject builder castToImageMenuItem) ["FileOpen","FileSaveAs","FileQuit"]
	aboutOpen <- builderGetObject builder castToImageMenuItem "aboutOpen"
	aboutCloseBtn <- builderGetObject builder castToButton "aboutCloseBtn"
	aboutDialog <- builderGetObject builder castToAboutDialog "aboutDialog"
	csvButton <- builderGetObject builder castToButton "csvButton"
	csvSaviour <- builderGetObject builder castToFileChooserDialog "csvSaveFileDialog"
	csvSaviourBtn <- builderGetObject builder castToButton "csvSaviourBtn"
	csvSaviourBtnCancel <- builderGetObject builder castToButton "csvSaviourBtnCancel"
	drawingArea1 <- builderGetObject builder castToDrawingArea "drawingarea1"
	drawingArea2 <- builderGetObject builder castToDrawingArea "drawingarea2"
	dropHBox <- builderGetObject builder castToHBox	"dropHBox"
	dynamiclabel<- builderGetObject builder castToLabel "dynamiclabel"
	entry1 <- builderGetObject builder castToEntry "entry1"
	formulaCombineBtn <- builderGetObject builder castToButton "formulaCombineBtn"
	formEntry1  <- builderGetObject builder castToEntry "formEntry1"
	formEntry2  <- builderGetObject builder castToEntry "formEntry2"
	formEntry3  <- builderGetObject builder castToEntry "formEntry3"
	formEntry4  <- builderGetObject builder castToEntry "formEntry4"
	formEntry5  <- builderGetObject builder castToEntry "formEntry5"
	formEntry6  <- builderGetObject builder castToEntry "formEntry6"
	formEntry7  <- builderGetObject builder castToEntry "formEntry7"
	formLbl1 <- builderGetObject builder castToLabel "formLbl1"
	formLbl2 <- builderGetObject builder castToLabel "formLbl2"
	formLbl3 <- builderGetObject builder castToLabel "formLbl3"
	formLbl4 <- builderGetObject builder castToLabel "formLbl4"
	formLbl5 <- builderGetObject builder castToLabel "formLbl5"
	formLbl6 <- builderGetObject builder castToLabel "formLbl6"
	formLbl7 <- builderGetObject builder castToLabel "formLbl7"
	formProBox1 <- builderGetObject builder castToComboBox "formProBox1"
	formProBox2 <- builderGetObject builder castToComboBox "formProBox2"
	formProBox3 <- builderGetObject builder castToComboBox "formProBox3"
	formProBox4 <- builderGetObject builder castToComboBox "formProBox4"
	formProBox5 <- builderGetObject builder castToComboBox "formProBox5"
	formProBox6 <- builderGetObject builder castToComboBox "formProBox6"
	formProBox7 <- builderGetObject builder castToComboBox "formProBox7"
	formProBox8 <- builderGetObject builder castToComboBox "formProBox8"
	hbox1 <- builderGetObject builder castToHBox "hbox1"
	hbox7 <- builderGetObject builder castToHBox "hbox7"
	hbox10 <- builderGetObject builder castToHBox "hbox10"
	hbox18 <- builderGetObject builder castToHBox "hbox18"
	hbox19 <- builderGetObject builder castToHBox "hbox19"
	hbox2 <- builderGetObject builder castToHBox "hbox2"
	liststore1 <- builderGetObject builder castToTreeModel "liststore1"
	mdlChk <- builderGetObject builder castToToolButton "mdlChkButton"
	modellabel <- builderGetObject builder castToLabel "modellabel"
	newBtn <- builderGetObject builder castToButton "newBtn"
	notebook1 <- builderGetObject builder castToNotebook "notebook1"
	notebook2 <- builderGetObject builder castToNotebook "notebook2"
	ode <- builderGetObject builder castToToolButton "odeButton"
	odelabel<- builderGetObject builder castToLabel "odelabel"
	opener <- builderGetObject builder castToFileChooserDialog "openFileDialog"
	openerBtn <- builderGetObject builder castToButton "openerBtn"
	openerBtnCancel <- builderGetObject builder castToButton "openerBtnCancel"
	parse <- builderGetObject builder castToToolButton "parseButton"
	phaseButton <- builderGetObject builder castToButton "phaseButton"
	phaselabel <- builderGetObject builder castToLabel "phaselabel"
	phaseSaviour <- builderGetObject builder castToFileChooserDialog "phaseSaveFileDialog"
	phaseSaveBtn <- builderGetObject builder castToButton "phaseSaveBtn"
	phaseSaviourBtn <- builderGetObject builder castToButton "phaseSaviourBtn"
	phaseSaviourBtnCancel <- builderGetObject builder castToButton "phaseSaviourBtnCancel"
	phaseSpecies1Combobox <- builderGetObject builder castToComboBox "phaseSpecies1Combobox"
	phaseSpecies2Combobox <- builderGetObject builder castToComboBox "phaseSpecies2Combobox"
	plotlabel <- builderGetObject builder castToLabel "plotlabel"
	plotSaveBtn <- builderGetObject builder castToButton "plotSaveBtn"
	plotSaviour <- builderGetObject builder castToFileChooserDialog "plotSaveFileDialog"
	plotSaviourBtn <- builderGetObject builder castToButton "plotSaviourBtn"
	plotSaviourBtnCancel <- builderGetObject builder castToButton "plotSaviourBtnCancel"
	rePlotBtn <- builderGetObject builder castToButton "rePlotBtn"
	saviour <- builderGetObject builder castToFileChooserDialog "saveFileDialog"
	saviourBtn <- builderGetObject builder castToButton "saviourBtn"
	saviourBtnCancel <- builderGetObject builder castToButton "saviourBtnCancel"
	simButton <- builderGetObject builder castToButton "simButton"
	simEndEntry  <- builderGetObject builder castToEntry "simEndEntry"
	simPointsEntry  <- builderGetObject builder castToEntry "simPointsEntry"
	simProComboBox <- builderGetObject builder castToComboBox "simProCombobox"
	simStartEntry  <- builderGetObject builder castToEntry "simStartEntry"
	staticlabel<- builderGetObject builder castToLabel "staticlabel"
	tablabel <- builderGetObject builder castToLabel "tablabel"
	treeView1 <- builderGetObject builder castToTreeView "treeView1"
	vbox5 <- builderGetObject builder castToVBox "vbox5"
	vbox8 <- builderGetObject builder castToVBox "vbox8"
	vbox13 <- builderGetObject builder castToVBox "vbox13"
	vbox14 <- builderGetObject builder castToVBox "vbox14"
	vbox15 <- builderGetObject builder castToVBox "vbox15"
	vbox6 <- builderGetObject builder castToVBox "vbox6"

	return $ GUI

		aboutOpen
		aboutCloseBtn
		aboutDialog
		csvButton
		csvSaviour
		csvSaviourBtn
		csvSaviourBtnCancel
		drawingArea1
		drawingArea2
		dropHBox
		dynamiclabel
		entry1
		mainMenu
		fileOpen
		fileQuit
		fileSaveAs
		formulaCombineBtn
		formEntry1
		formEntry2
		formEntry3
		formEntry4
		formEntry5
		formEntry6
		formEntry7
		formLbl1
		formLbl2
		formLbl3
		formLbl4
		formLbl5
		formLbl6
		formLbl7
		formProBox1
		formProBox2
		formProBox3
		formProBox4
		formProBox5
		formProBox6
		formProBox7
		formProBox8
		hbox1
		hbox7
		hbox10
		hbox18
		hbox19
		hbox2
		liststore1
		mdlChk
		modellabel
		mw
		newBtn
		notebook1
		notebook2
		ode
		odelabel
		opener
		openerBtn
		openerBtnCancel
		parse
		phaseButton
		phaselabel
		phaseSaveBtn
		phaseSaviour
		phaseSaviourBtn
		phaseSaviourBtnCancel
		phaseSpecies1Combobox
		phaseSpecies2Combobox
		plotlabel
		plotSaveBtn
		plotSaviour
		plotSaviourBtn
		plotSaviourBtnCancel
		rePlotBtn
		saviour
		saviourBtn
		saviourBtnCancel
		simButton
		simEndEntry
		simPointsEntry
		simProComboBox
		simStartEntry
		staticlabel
		tablabel
		treeView1
		vbox5
		vbox8
		vbox13
		vbox14
		vbox15
		vbox6



data GUI = GUI {

	aboutOpen :: ImageMenuItem,
	aboutCloseBtn :: Button,
	aboutDialog :: AboutDialog,
	csvButton :: Button,
	csvSaviour :: FileChooserDialog,
	csvSaviourBtn :: Button,
	csvSaviourBtnCancel :: Button,
	drawingArea1 :: DrawingArea,
	drawingArea2 :: DrawingArea,
	dropHBox :: HBox,
	dynamiclabel :: Label,
	entry1 :: Entry,
	mainMenu :: MenuBar,
	fileOpen :: ImageMenuItem,
	fileQuit :: ImageMenuItem,
	fileSaveAs :: ImageMenuItem,
	formulaCombineBtn :: Button,
	formEntry1 :: Entry,
	formEntry2 :: Entry,
	formEntry3 :: Entry,
	formEntry4 :: Entry,
	formEntry5 :: Entry,
	formEntry6 :: Entry,
	formEntry7 :: Entry,
	formLbl1 :: Label,
	formLbl2 :: Label,
	formLbl3 :: Label,
	formLbl4 :: Label,
	formLbl5 :: Label,
	formLbl6 :: Label,
	formLbl7 :: Label,
	formProBox1 :: ComboBox,
	formProBox2 :: ComboBox,
	formProBox3 :: ComboBox,
	formProBox4 :: ComboBox,
	formProBox5 :: ComboBox,
	formProBox6 :: ComboBox,
	formProBox7 :: ComboBox,
	formProBox8 :: ComboBox,
	hbox1 :: HBox,
	hbox7 :: HBox,
	hbox10 :: HBox,
	hbox18 :: HBox,
	hbox19 :: HBox,
	hbox2 :: HBox,
	liststore1 :: TreeModel,
	mdlChk :: ToolButton,
	modellabel :: Label,
	mainWin :: Window,
	newBtn :: Button,
	notebook1 :: Notebook,
	notebook2 :: Notebook,
	ode :: ToolButton,
	odelabel :: Label,
	opener :: FileChooserDialog,
	openerBtn :: Button,
	openerBtnCancel :: Button,
	parse :: ToolButton,
	phaseButton :: Button,
	phaselabel :: Label,
	phaseSaveBtn :: Button,
	phaseSaviour :: FileChooserDialog,
	phaseSaviourBtn :: Button,
	phaseSaviourBtnCancel :: Button,
	phaseSpecies1Combobox :: ComboBox,
	phaseSpecies2Combobox :: ComboBox,
	plotlabel :: Label,
	plotSaveBtn :: Button,
	plotSaviour :: FileChooserDialog,
	plotSaviourBtn :: Button,
	plotSaviourBtnCancel :: Button,
	rePlotBtn :: Button,
	saviour :: FileChooserDialog,
	saviourBtn :: Button,
	saviourBtnCancel :: Button,
	simButton :: Button,
	simEndEntry :: Entry,
	simPointsEntry :: Entry,
	simProComboBox :: ComboBox,
	simStartEntry :: Entry,
	staticlabel :: Label,
	tablabel :: Label,
	treeView1 :: TreeView,
	vbox5 :: VBox,
	vbox8 :: VBox,
	vbox13 :: VBox,
	vbox14 :: VBox,
	vbox15 :: VBox,
	vbox6 :: VBox
    }




connectGui :: GUI ->   SourceView -> IO (ConnectId Button)
connectGui gui  mainText =
    do
	on  (mainWin gui) objectDestroy mainQuit
        on (fileOpen gui)  menuItemActivated $ windowPresent $ opener gui

        on (fileSaveAs gui)  menuItemActivated $ saveAs gui
        on (fileQuit gui) menuItemActivated mainQuit
	on (aboutOpen gui)  menuItemActivated $ windowPresent $ aboutDialog gui
        on (aboutCloseBtn gui)  buttonActivated $ widgetHide $ aboutDialog gui


        onToolButtonClicked (parse gui ) $ parseClicked gui  mainText
 	onToolButtonClicked (ode gui) $ odeClicked gui
	on (simButton gui)  buttonActivated ( do
					plotClicked gui
					tableClicked gui
					comboBoxSetActive (phaseSpecies1Combobox gui) 0
					comboBoxSetActive (phaseSpecies2Combobox gui) 1
					phaseClicked gui
					)

        onToolButtonClicked (mdlChk gui) $ modelCheckClicked gui
	on (csvButton gui)  buttonActivated $ saveCSVas  gui
	on (phaseButton gui)  buttonActivated $ phaseClicked gui
--opener
        on (openerBtn gui)  buttonActivated $ openerBtnClicked gui mainText
        on (openerBtnCancel gui)  buttonActivated $ widgetHide $ opener gui
--saver
        on (saviourBtn gui)  buttonActivated $ saviourSaveBtnClicked gui mainText
        on (csvSaviourBtn gui)  buttonActivated $ csvSaviourSaveBtnClicked  gui


        on (saviourBtnCancel gui)  buttonActivated $ widgetHide $ saviour gui
        on (csvSaviourBtnCancel gui)  buttonActivated $ widgetHide $ csvSaviour gui

	on (newBtn gui )  buttonActivated $ alter gui




alter :: GUI ->  IO()
alter gui  =
	do
	sourceView <- sourceViewNew
	widgetShowAll (vbox14 gui)
	containerRemove (dropHBox gui) (newBtn gui)
	collectComboT gui (hbox19 gui)  sourceView



removeHBoxT :: GUI -> HBox -> HBox ->  SourceView -> IO()
removeHBoxT gui line hb  sv  =
			do
			p <- GTK.get line (boxChildPosition (dropHBox gui)  )

			if p < 2 then do
					containerRemove (dropHBox gui) hb
					initComboT gui line  sv
			else do
				containerRemove (dropHBox gui) hb
				initComboTV gui line  sv


removeHBox :: GUI -> HBox -> HBox ->  SourceView -> IO()
removeHBox gui line  hb  sv =
			do
			containerRemove (dropHBox gui) hb
			initComboTV gui line  sv

removeHBoxO:: GUI -> HBox -> HBox ->  SourceView -> IO()
removeHBoxO gui line  hb  sv =
			do
			containerRemove (dropHBox gui) hb
			initComboO gui line  sv

removeHBoxV:: GUI -> HBox -> HBox ->  SourceView -> IO()
removeHBoxV gui line  hb  sv =
			do
			containerRemove (dropHBox gui) hb
			initComboV gui line  sv

collectTim :: GUI -> HBox -> HBox -> ComboBox ->  Entry -> Entry ->  SourceView -> IO()
collectTim gui line hb cb f t   sourceView = do

	n <- comboBoxGetActive cb
	if (n == -1) then return()
	else do
				from <- entryGetText f
				if (isInteger from) then do
					to <- entryGetText t
					if (isInteger to) && ((read to::Int) > (read from::Int)) then do
						endIt <- entryGetText (entry1 gui) :: IO (String)
						let c = ["F" ,"G"]
						let str = c!!(n) ++ "{" ++ from ++ "," ++ to ++ "} ( "
						timLbl <- labelNew (Nothing :: Maybe(String))
						labelSetText timLbl str
						entryAppendText (entry1 gui) str
						box <- containerGetChildren (dropHBox gui)
						mapM_ (containerRemove (dropHBox gui)) box
						leftBtn <- buttonNewWithLabel "<-"
						on leftBtn  buttonActivated $ removeLblT gui line leftBtn timLbl  sourceView endIt
						boxPackStart line leftBtn PackNatural 0
						boxReorderChild line (dropHBox gui) 0
						boxPackStart line timLbl PackNatural 0
						initComboTV gui line  sourceView
						set line [boxChildPacking leftBtn := PackNatural, boxChildPacking timLbl := PackNatural]
						boxReorderChild line (dropHBox gui) 1
						box2 <- containerGetChildren hb
						let braks =  div (length box2 -1) 2
						buff <- textViewGetBuffer sourceView
						si <- textBufferGetStartIter buff
        					ei <- textBufferGetEndIter buff
						let i = take braks (repeat ')')
						text <- textBufferGetText buff si ei False
						textBufferSetText buff (text ++ str )

						let x = (length box2)
						boxReorderChild line (dropHBox gui) x
						widgetShowAll line
					else return()
				else return()

removeLbl gui line leftBtn lbl  sv = do
					containerRemove line leftBtn
					containerRemove line lbl
					widgetShowAll line
					initComboTV gui line  sv

removeLblO gui line leftBtn lbl  sv endIt = do


					str <- labelGetText lbl
					txt <- entryGetText (entry1 gui)
					let t = Utils.replace  str " " txt
					let txt = t
					entrySetText (entry1 gui) endIt
					p <- GTK.get line (boxChildPosition leftBtn)
					box <- containerGetChildren line

					let b = slice p (length(box) -2) box
					mapM_ (containerRemove line) b
					widgetShowAll line
					box <- containerGetChildren line
					if (length box) > 2 then do
								box2 <- containerGetChildren (dropHBox gui)
								mapM_ (containerRemove (dropHBox gui)) box2
								initComboO gui line  sv
					else
						return ()

slice from to xs = take (to - from + 1) (drop from xs)


removeLblT gui line leftBtn timLbl  sv endIt = do
					str <- labelGetText timLbl
					txt <- entryGetText (entry1 gui)
					let t = Utils.replace  str " " txt
					let txt = t
					entrySetText (entry1 gui) endIt
					p <- GTK.get line (boxChildPosition leftBtn)
					box <- containerGetChildren line
					let b = slice p (length(box) - 2) box
					mapM_ (containerRemove line) b
					widgetShowAll line
					box <- containerGetChildren line
					let n = (length box)
					if (n > 2) then do
						box2 <- containerGetChildren (dropHBox gui)
						mapM_ (containerRemove (dropHBox gui)) box2
						initComboTV gui line  sv
					else do
						box2 <- containerGetChildren (dropHBox gui)
						mapM_ (containerRemove (dropHBox gui)) box2
						initComboT gui line  sv

removeLblV gui line leftBtn timLbl  sv endIt = do
					str <- labelGetText timLbl
					txt <- entryGetText (entry1 gui)
					let t = Utils.replace  str " " txt
					let txt = t
					entrySetText (entry1 gui) endIt
					p <- GTK.get line (boxChildPosition leftBtn)
					box <- containerGetChildren line
					let b = slice p (length(box) - 2) box
					mapM_ (containerRemove line) b
					widgetShowAll line
					box <- containerGetChildren line
					let n = (length box)
					if (n > 2) then do
						box2 <- containerGetChildren (dropHBox gui)
						containerRemove (dropHBox gui) (box2!!0)
						box2 <- containerGetChildren (dropHBox gui)
						initComboV gui line  sv
					else return()



initComboTV :: GUI -> HBox ->  SourceView -> IO ()
initComboTV gui line  sv  = do
			cb <- comboBoxNewText
			comboBoxAppendText cb (T.pack "Time")
			comboBoxAppendText cb (T.pack "Value")
			boxPackStart (dropHBox gui) cb PackNatural 0
			on cb  changed $ collectComboTV gui line cb  sv
			widgetShowAll (dropHBox gui)

initComboT :: GUI -> HBox ->  SourceView -> IO ()
initComboT gui line  sv = do
			cb <- comboBoxNewText
			comboBoxAppendText cb (T.pack "Time")
			boxPackStart (dropHBox gui) cb PackNatural 0
			cb `on` changed $ collectComboT gui line  sv
			widgetShowAll (dropHBox gui)

initComboV :: GUI -> HBox ->  SourceView -> IO ()
initComboV gui line   sv = do
			cb <- comboBoxNewText
			comboBoxAppendText cb (T.pack "Value")
			boxPackEnd (dropHBox gui) cb PackNatural 0
			cb `on` changed $ collectComboV gui line  sv
			widgetShowAll (dropHBox gui)

initComboVfromO :: GUI -> HBox ->  SourceView -> IO ()
initComboVfromO gui line   sv = do
			cb <- comboBoxNewText
			comboBoxAppendText cb (T.pack "Value")
			boxPackEnd (dropHBox gui) cb PackNatural 0
			cb `on` changed $ collectComboVfromO gui line  sv
			widgetShowAll (dropHBox gui)


initComboO :: GUI -> HBox ->  SourceView -> IO ()
initComboO gui line   sv = do
			cb <- comboBoxNewText
			box <- containerGetChildren line
			text <- entryGetText (entry1 gui)
			let n = T.count (T.singleton '(') (T.pack text)
			let str = take n (repeat ')')
			let possForm = ( text ++ " " ++ str)
			case parseFormula (possForm) of
				Left err -> do
						comboBoxAppendText cb (T.pack "Operator")
		                Right f  -> do
					comboBoxAppendText cb (T.pack "Operator")
					comboBoxAppendText cb (T.pack "Done")
					comboBoxAppendText cb (T.pack "Or")

			boxPackEnd (dropHBox gui) cb PackNatural 0
			cb `on` changed $ collectComboO gui line  sv cb
			widgetShowAll (dropHBox gui)

collectComboO :: GUI -> HBox ->  SourceView ->  ComboBox -> IO()
collectComboO gui line  sv cb = do
				n <- comboBoxGetActive cb

				if (n == 1) then do
					removeAll gui line  sv
				else if (n == 0) then do
					hb <- hBoxNew False 3
					box <- containerGetChildren (dropHBox gui)
					mapM_ (containerRemove (dropHBox gui)) box
					comboAddOp gui line hb  sv
					boxPackStart (dropHBox gui) hb  PackNatural 0
				else do
					hb <- hBoxNew False 3
					fLbl <- labelNew (Just "Or")
					boxPackStart hb fLbl  PackNatural 0
					text <- entryGetText (entry1 gui)
					let n = T.count (T.singleton '(') (T.pack text)
					let str = take n (repeat ')')
					entryAppendText (entry1 gui) (" " ++ str ++ ") || ( ")
					text <- entryGetText (entry1 gui)
					entrySetText (entry1 gui) ("( " ++ text)
					box <- containerGetChildren (dropHBox gui)
					mapM_ (containerRemove (dropHBox gui)) box
					containerRemove line (dropHBox gui)
					boxPackStart  hb (dropHBox gui)  PackNatural 0
					initComboTV gui hb  sv
					boxPackStart (vbox14 gui) hb PackNatural 0
					set (vbox14 gui) [boxChildPacking hb := PackNatural]
					packing1 <- boxQueryChildPacking hb (dropHBox gui)
					set hb [boxChildPacking (dropHBox gui) := PackNatural, boxChildPacking fLbl := PackNatural]
					widgetShowAll line
					boxReorderChild (vbox14 gui) hb 1
					widgetShowAll (vbox14 gui)



removeAll gui line  sv = do
			Just tds <- get (parse gui) modelDef 				;
			text <- entryGetText (entry1 gui)
			let n = T.count (T.singleton '(') (T.pack text)
			let m = T.count ( T.pack "||") ( T.pack text)

			if (m > 0) then do
				box <- containerGetChildren (vbox14 gui)
				containerRemove (vbox14 gui) (box!!((m-1)))
			else
				return()
			let str = take n (repeat ')')
			entryAppendText (entry1 gui) (" " ++ str)
			box <- containerGetChildren line
			let b = slice 0 (length(box) -2) box
			mapM_ (containerRemove line) b
			widgetShowAll line
			box2 <- containerGetChildren (dropHBox gui)
			mapM_ (containerRemove (dropHBox gui)) box2
			initComboT gui line  sv
			t <- entryGetText (entry1 gui)
			entrySetText (entry1 gui) ""
			newEntry <- entryNew
			entryAppendText newEntry t
			hb <- hBoxNew False 3
			fBtn <- buttonNewWithLabel "clear"
			fLbl <- labelNew (Just "outcome")
			boxPackStart hb newEntry  PackNatural 0
			boxPackStart hb fLbl PackNatural 0
			boxPackStart hb fBtn  PackNatural 0
			connectFormula newEntry fLbl tds
			widgetSetSizeRequest hb 0 20
 			set hb [boxChildPacking fBtn := PackNatural, boxChildPacking fLbl := PackNatural, boxChildPacking newEntry := PackGrow
				]

			boxPackStart (vbox14 gui) hb PackNatural 0
			fBtn `on` buttonActivated $ containerRemove (vbox14 gui) hb
			widgetShowAll (vbox14 gui)


--connectFormula entry btn
connectFormula entry lbl tds = do
				let ps = pros tds;
				let p = ps!!(0);
				form <- entryGetText entry
				case parseFormula (form) of
		                       		Left err -> do
							let text  = "error"
							labelSetText lbl text
		                      		Right f  -> do
							let f' = reconcileSpecs tds f
		                                  	let text = modelCheckSig tds
		                                                 solveODE
		                                                 Nothing
		                                                 p
		                              			 (500,(0,2))
		                                                 f' --}
							labelSetText lbl (show text)



comboAddOp :: GUI -> HBox -> HBox ->  SourceView -> IO()
comboAddOp gui line hb  sv = do
	cb <- comboBoxNewText
	addOp cb
	cb `on` changed $ hotSwapComboO gui line cb hb  sv
	leftBtn <- buttonNewWithLabel "<-"
	boxPackStart hb leftBtn  PackNatural 0
	set hb [boxChildPacking leftBtn := PackNatural]
	boxPackStart hb cb PackNatural 0
	leftBtn `on` buttonActivated $ removeHBoxO gui line hb  sv
	widgetShowAll hb



comboAddVal :: GUI -> HBox -> HBox ->  SourceView -> IO()
comboAddVal gui line hb  sv = do
	cb <- comboBoxNewText
	addVal cb
	cb `on` changed $ hotSwapComboTV gui line cb hb  sv
	boxPackStart hb cb  PackNatural 0
	widgetShowAll hb


comboAddValfromO :: GUI -> HBox -> HBox ->  SourceView -> IO()
comboAddValfromO gui line hb  sv = do
	cb <- comboBoxNewText
	addVal cb

	cb `on` changed $ hotSwapComboTV gui line cb hb  sv
	boxPackStart hb cb PackNatural 0
	widgetShowAll hb



hotSwapComboO :: GUI -> HBox -> ComboBox -> HBox ->  SourceView -> IO()
hotSwapComboO gui line cb hb  sv = do
			i <- comboBoxGetActive cb
			box <- containerGetChildren hb
			if length(box) > 3 then do
				containerRemove hb (box!!(2))
				containerRemove hb (box!!(3))
			else return()
			if i == 0 then do
					cb <- comboBoxNewText
					addRel cb
					relBtn <- buttonNewWithLabel "Add"
		 			relBtn  `on` buttonActivated $ collectOpR gui line  cb sv
					boxPackStart hb cb  PackNatural 0
					boxPackStart hb relBtn PackNatural 0
					boxReorderChild hb cb 2
					widgetShowAll hb
			else do
					cb <- comboBoxNewText
					addAri cb
					ariBtn <- buttonNewWithLabel "Add"
		 			ariBtn  `on` buttonActivated $ collectOpA gui line  cb sv
					boxPackStart hb cb  PackNatural 0
					boxPackStart hb ariBtn PackNatural 0
					boxReorderChild hb cb 2
					widgetShowAll hb


collectOpR :: GUI -> HBox ->   ComboBox -> SourceView -> IO()
collectOpR gui line  cb sv = do
				i <- comboBoxGetActive cb
				endIt <- entryGetText (entry1 gui) :: IO(String)
				let optns = [" > "," < "," = "]
				opLbl <- labelNew (Nothing :: Maybe String)
				let str =  optns!!(i)
				labelSetText opLbl str
				entryAppendText (entry1 gui) str
				box <- containerGetChildren (dropHBox gui)
				mapM_ (containerRemove (dropHBox gui)) box
				leftBtn <- buttonNewWithLabel "<-"
				leftBtn `on` buttonActivated $ removeLblO gui line leftBtn opLbl  sv endIt
				initComboVfromO gui line  sv
				boxPackStart line leftBtn PackNatural 0

				boxReorderChild line (dropHBox gui) 0
				boxPackStart line opLbl PackNatural 0
				set line [boxChildPacking leftBtn := PackNatural, boxChildPacking opLbl := PackNatural ]
				boxReorderChild line (dropHBox gui) 1
				box2 <- containerGetChildren line
				let braks =  div (length(box2)-1) 2
				buff <- textViewGetBuffer sv
				si <- textBufferGetStartIter buff
        			ei <- textBufferGetEndIter buff
				let i = take braks (repeat ')')
				text <- textBufferGetText buff si ei False
				textBufferSetText buff (text ++ str )
				let x = (length box2)
				boxReorderChild line (dropHBox gui) x
				widgetShowAll line


collectOpA :: GUI -> HBox ->  ComboBox  -> SourceView -> IO()
collectOpA gui line  cb sv = do
				i <- comboBoxGetActive cb
				endIt <- entryGetText (entry1 gui) :: IO(String)
				let optns = [" + "," - "," x "," / "]
				opLbl <- labelNew (Nothing :: Maybe(String))
				labelSetText opLbl $ optns!!(i)
				entryAppendText (entry1 gui) $ optns!!(i)

				box <- containerGetChildren (dropHBox gui)
				mapM_ (containerRemove (dropHBox gui)) box
				leftBtn <- buttonNewWithLabel "<-"
				leftBtn `on` buttonActivated $ removeLblO gui line leftBtn opLbl  sv endIt
				initComboVfromO gui line  sv
				boxPackStart line leftBtn PackNatural 0

				boxReorderChild line (dropHBox gui) 0
				boxPackStart line opLbl	 PackNatural 0
				set line [boxChildPacking leftBtn := PackNatural, boxChildPacking opLbl := PackNatural ]
				boxReorderChild line (dropHBox gui) 1
				box2 <- containerGetChildren line
				let x = (length box2)
				boxReorderChild line (dropHBox gui) x
				widgetShowAll line




collectComboTV :: GUI -> HBox  -> ComboBox ->  SourceView -> IO()
collectComboTV gui line cb  sv  = do
			i <- comboBoxGetActive cb
			box <- containerGetChildren (dropHBox gui)
			if length(box) > 1 then do
				containerRemove (dropHBox gui) (box!!(1))
			else return()
			if i == 0 then do
					collectComboT gui line  sv
			else do
				collectComboV gui line  sv

collectComboT :: GUI -> HBox ->  SourceView -> IO()
collectComboT gui line  sv = do
			box <- containerGetChildren (dropHBox gui)

			if length(box) > 1 then do
				containerRemove (dropHBox gui) (box!!(1))
			else return()
			comboAddTim gui line  sv


comboAddTim :: GUI -> HBox ->  SourceView-> IO()
comboAddTim gui line  sv =
	do
	hb <- hBoxNew False 3
	cb <- comboBoxNewText
	addTim cb
	fromTim <- entryNew
	entrySetText fromTim "From"
	toTim <- entryNew
	entrySetText toTim "To"
	timBtn <- buttonNewWithLabel "Add"
	set fromTim [ widgetWidthRequest := 50]
	set toTim [ widgetWidthRequest := 50]
	boxPackStart hb cb PackNatural 0
	boxPackStart hb fromTim PackNatural 0
	boxPackStart hb toTim PackNatural 0
	boxPackStart hb timBtn PackNatural 0
	set hb [ boxChildPacking fromTim := PackNatural, boxChildPacking toTim := PackNatural, boxChildPacking timBtn := PackNatural, boxChildPacking cb := PackNatural ]
	timBtn  `on` buttonActivated $ collectTim gui line hb cb fromTim toTim  sv
	boxPackEnd (dropHBox gui) hb  PackNatural 0
	widgetShowAll hb



collectComboV :: GUI -> HBox ->  SourceView -> IO()
collectComboV gui line  sv = do
			hb <- hBoxNew False 3
			box <- containerGetChildren (dropHBox gui)
			containerRemove (dropHBox gui) (box!!(0))
			comboAddVal gui line hb  sv
			boxPackStart (dropHBox gui) hb PackNatural 0

collectComboVfromO :: GUI -> HBox ->  SourceView -> IO()
collectComboVfromO gui line  sv = do
			hb <- hBoxNew False 3
			box <- containerGetChildren (dropHBox gui)
			containerRemove (dropHBox gui) (box!!(0))
			comboAddValfromO gui line hb  sv
			boxPackStart (dropHBox gui) hb PackNatural 0



collectValS :: GUI -> HBox ->   [String] -> ComboBox -> SourceView -> IO()
collectValS gui line  sns cb sv = do
				n <- comboBoxGetActive cb
				endIt <- entryGetText (entry1 gui) :: IO (String)
				if (n /= -1) then do
					let s = sns!!(n)
					specLbl <- labelNew (Nothing :: Maybe(String))
					let str =  " [ " ++ s ++ " ] "
					labelSetText specLbl str
					entryAppendText (entry1 gui) $ " [ " ++ s ++ " ] "
					box <- containerGetChildren (dropHBox gui)
					mapM_ (containerRemove (dropHBox gui)) box
					leftBtn <- buttonNewWithLabel "<-"
					leftBtn `on` buttonActivated $ removeLblV gui line leftBtn specLbl  sv endIt
					initComboO gui line  sv
					boxPackStart line leftBtn PackNatural 0

					boxReorderChild line (dropHBox gui) 0
					boxPackStart line specLbl PackNatural 0
					set line [boxChildPacking leftBtn := PackNatural, boxChildPacking specLbl := PackNatural]
					boxReorderChild line (dropHBox gui) 1
					box2 <- containerGetChildren line
					let braks =  div (length(box2)-1) 2
					buff <- textViewGetBuffer sv
					si <- textBufferGetStartIter buff
					ei <- textBufferGetEndIter buff
					let i = take braks (repeat ')')
					text <- textBufferGetText buff si ei False
					textBufferSetText buff (text ++ str )
					let x = (length box2)
					boxReorderChild line (dropHBox gui) x
					widgetShowAll line
				else return()


entryAppendText :: Entry -> String -> IO()
entryAppendText entry str = do
				tmpstr <- entryGetText entry
				entrySetText entry  ( tmpstr ++str)



collectValN :: GUI -> HBox  -> HBox -> Entry ->  SourceView -> IO()
collectValN gui line hb valEntry   sv = do
	val <- entryGetText valEntry
	endIt <- entryGetText (entry1 gui) :: IO (String)
	if (isInteger val) then do
		valLbl <- labelNew (Nothing :: Maybe(String))
		labelSetText valLbl val
		entryAppendText (entry1 gui) val
		box <- containerGetChildren (dropHBox gui)
		mapM_ (containerRemove (dropHBox gui)) box
		leftBtn <- buttonNewWithLabel "<-"
		leftBtn `on` buttonActivated $ removeLblV gui line leftBtn valLbl  sv endIt
		initComboO gui line  sv
		boxPackStart line leftBtn PackNatural 0

		boxReorderChild line (dropHBox gui) 0
		boxPackStart line valLbl PackNatural 0
		set line [boxChildPacking leftBtn := PackNatural, boxChildPacking valLbl := PackNatural]
		boxReorderChild line (dropHBox gui) 1
		box2 <- containerGetChildren line
		let braks =  div (length(box2)-1) 2
		buff <- textViewGetBuffer sv
		si <- textBufferGetStartIter buff
		ei <- textBufferGetEndIter buff
		let i = take braks (repeat ')')
		text <- textBufferGetText buff si ei False
		textBufferSetText buff (text ++ val )
		let x = (length box2)
		boxReorderChild line (dropHBox gui) x
		widgetShowAll line
	else return()




hotSwapComboTV :: GUI -> HBox -> ComboBox -> HBox ->  SourceView -> IO()
hotSwapComboTV gui line cb hb  sv = do
			i <- comboBoxGetActive cb
			box <- containerGetChildren (dropHBox gui)
			box <- containerGetChildren hb

			if length(box) > 1 then do
				containerRemove hb (box!!(1))
				containerRemove hb (box!!(2))
			else return()
			if i == 0 then do
					Just tds <- get (parse gui) modelDef 				;
					let ps = pros tds;
					let pns = proNames tds
					let p = ps!!(0);
					let ss = speciesInProc p
					let sns = specNames tds
					cb <- comboBoxNewText
					addSpecs cb (map T.pack sns) (length sns) (length sns)
					valBtn <- buttonNewWithLabel "Add"
					valBtn  `on` buttonActivated $ collectValS gui line sns cb sv
					boxPackStart hb cb PackNatural 0
					boxPackEnd hb valBtn PackNatural 0
					boxReorderChild hb cb 2
					widgetShowAll hb
			else do
				valEntry <- entryNew
				entrySetText valEntry "enter value"
				set valEntry [ widgetWidthRequest := 90]
				valBtn <- buttonNewWithLabel "Add"

				valBtn  `on` buttonActivated $ collectValN gui line  hb valEntry  sv
				boxPackStart hb valEntry PackNatural 0
				boxPackEnd hb valBtn  PackNatural 0
				set hb [ boxChildPacking valEntry := PackNatural]

				boxReorderChild hb valEntry 2
				widgetShowAll hb





openerBtnClicked :: GUI -> SourceView -> IO ()
openerBtnClicked gui mainText =
    do
        file <- fileChooserGetFilename (opener gui)
        case file of
            Just fpath -> loadFile (show fpath) gui
            Nothing -> widgetHide (opener gui)
        where
            loadFile fileName gui =
                do
                    inf <- openFile (init  (tail fileName)) ReadMode
                    inputData <- hGetContents inf
                    buff <- textViewGetBuffer mainText
                    textBufferSetText buff inputData

		    set (mainWin gui) [ windowTitle :=   (init(tail fileName)) ]

                    hClose inf
                    widgetHide (opener gui)


--saver window

saveAs :: GUI -> IO ()
saveAs gui =
    do
        fileChooserSetAction (saviour gui) FileChooserActionSave
        windowPresent (saviour gui)

saveCSVas ::  GUI -> IO ()
saveCSVas  gui =
    do
	box <- containerGetChildren (hbox2 gui)
	if ((length box) > 0) then do
				fileChooserSetAction (csvSaviour gui) FileChooserActionSave
        			windowPresent (csvSaviour gui)
	else do return()

csvSaviourSaveBtnClicked ::  GUI -> IO ()
csvSaviourSaveBtnClicked  gui =
    do
        file <- fileChooserGetFilename (csvSaviour gui)
        case file of
            Just fpath -> do
				csvSaveClicked (init(tail(show fpath))) gui
				widgetHide (csvSaviour gui)
            Nothing -> widgetHide (opener gui)

--plotSave ::  GUI -> IO ()
plotSave gui ts' solns ss ss' =
    do
	(plotSaviourBtn gui) `on` buttonActivated $ plotSaviourBtnClicked  gui ts' solns ss ss'
	fileChooserSetAction (plotSaviour gui) FileChooserActionSave
        windowPresent (plotSaviour gui)



--plotSaviourBtnClicked ::  GUI -> IO()
plotSaviourBtnClicked  gui ts' solns ss ss' =
	do
		file <- fileChooserGetFilename (plotSaviour gui)
        	case file of
           		Just fpath -> do
				plotFilterAndSave (init(tail(show fpath))) gui  ts' solns ss ss'
				widgetHide (plotSaviour gui)
            		Nothing -> widgetHide (opener gui)

plotFilterAndSave file gui ts solns ss ss' = do
		Just a <- get (rePlotBtn gui) plotList
		let blss = zip a ss
		let fs = specFill blss
		plotTimeSeriesToFile ts solns ss  file


phaseSave gui ts' solns ss ss' =
    do
	(phaseSaviourBtn gui) `on` buttonActivated $ phaseSaviourBtnClicked  gui ts' solns ss ss'
	fileChooserSetAction (phaseSaviour gui) FileChooserActionSave
        windowPresent (phaseSaviour gui)



--plotSaviourBtnClicked ::  GUI -> IO()
phaseSaviourBtnClicked  gui ts' solns ss ss' =
	do
		file <- fileChooserGetFilename (phaseSaviour gui)
        	case file of
           		Just fpath -> do
                -- TODO: restore function
				phasePlot2ToFile ts' solns ss ss' (init $ tail $ show fpath)
				--plotFilterAndSave (init(tail(show fpath))) gui  ts' solns ss'
				widgetHide (phaseSaviour gui)
            		Nothing -> widgetHide (opener gui)





saviourSaveBtnClicked :: GUI -> SourceView -> IO ()
saviourSaveBtnClicked gui mainText =
    do
        file <- fileChooserGetFilename (saviour gui)
        case file of
            Just fpath -> save (init(tail(show fpath))) gui mainText
            Nothing -> widgetHide (opener gui)


save :: FilePath -> GUI -> SourceView -> IO ()
save fileName gui mainText =
    do
        outh <- openFile fileName WriteMode
        buff <- textViewGetBuffer mainText
        si <- textBufferGetStartIter buff
        ei <- textBufferGetEndIter buff
        text <- textBufferGetText buff si ei False
        hPutStrLn outh text
        hClose outh
        widgetHide (saviour gui)
	set (mainWin gui) [ windowTitle :=   fileName ]
	textBufferSetModified buff False

modelDef :: Attr ToolButton (Maybe ([Definition]))
modelDef = unsafePerformIO $ objectCreateAttribute




parseClicked :: GUI ->  SourceView -> IO ()
parseClicked gui  mainText =
	    do
		coltemp <- treeViewGetColumns (treeView1 gui)
		modelList <- listStoreNew (["Charlie","Delta"])
		treeViewSetModel (treeView1 gui) modelList
		buff <- textViewGetBuffer mainText
		si <- textBufferGetStartIter buff
		ei <- textBufferGetEndIter buff
		text <- textBufferGetText buff si ei False
		widgetShowAll (vbox13 gui)
		case parseFile text of
			Left err -> do
				box <- containerGetChildren (hbox18 gui)
				mapM_ (containerRemove (hbox18 gui)) box
				box2 <- containerGetChildren (vbox8 gui)
				mapM_ (containerRemove (vbox8 gui)) box2
				errLbl <- labelNew ( Just (show err))
				boxPackStart (hbox18 gui) errLbl PackNatural 0
				widgetShowAll (hbox18 gui)
			Right ds -> do
				widgetSetSensitive (vbox15 gui) True
				box <- containerGetChildren (hbox18 gui)
				mapM_ (containerRemove (hbox18 gui)) box
				set (parse gui) [ modelDef := (Just ds)]
				Just tds <- get (parse gui) modelDef


				let ps = fullpros tds
				let ss = specs tds
				let pss = (prettys ps)
				let psss = (splitOn "||" pss)
				let pssss = concat $ map tail $ map (splitOn "[" ) $ map head $ map (splitOn "]" ) psss
				let cs = pssss ++ [" "]
				let ls = lines(prettys tds)
				let [s,d] = transpose (map (splitOn "=") ls)

				--listStoreAppend (liststore1 gui) "Beta"
				addTreeview s gui (hbox18 gui) "Species" 60
				addTreeview d gui (hbox18 gui) "Definitions" 200
				addTreeview cs gui (hbox18 gui) "Concentration" 60
				onCom text s gui
				widgetShowAll (hbox18 gui)
				widgetShowAll (odelabel gui)
				widgetShowAll (vbox15 gui)


addTreeview ls gui w str n  = do
		list <- listStoreNew ls
		treeview <- Model.treeViewNewWithModel list
		Model.treeViewSetHeadersVisible treeview True
	    	col <- Model.treeViewColumnNew
		Model.treeViewColumnSetTitle col str
		renderer <- Model.cellRendererTextNew
		Model.cellLayoutPackStart col renderer False
		Model.cellLayoutSetAttributes col renderer list
			$ \ind -> [Model.cellText := ind]
		Model.treeViewAppendColumn treeview col
		treeViewColumnSetMaxWidth col n
		boxPackStart w treeview PackNatural 0


onCom text specs gui = do
		let lionel = lines text
		let oc = onlinecomments lionel
		let pieces = map (L.\\ words ("-- BioModel is")) oc
		let pieces2 = transpose pieces
		let nWSSpecs = map strip specs
		let onlinesp =  transpose $ matchSpecsPieces pieces nWSSpecs
		let onlines = emptyonlines onlinesp (length specs)
		addTreeview (onlines!!(1)) gui (hbox18 gui) "Name" 50
		let ids = (onlines!!(0))
		let us = urls (onlines!!(0))
		let lbs = linkbtns ids us
		box <- containerGetChildren (vbox8 gui)
		mapM_ (containerRemove (vbox8 gui)) box
		linkLbl <- labelNew (Just "Link")
		set linkLbl [ widgetHeightRequest := 22]
		boxPackStart (vbox8 gui) linkLbl PackNatural 0
		set (vbox8 gui) [ boxChildPacking linkLbl := PackNatural]
		meh ids us (vbox8 gui)
		widgetShowAll (notebook1 gui)

emptyonlines [] n = [(replicate n ""), (replicate n "")]
emptyonlines onlines n =  onlines



urls [] = []
urls (x:xs) =	if ((length x) == 10) then
				("http://www.ebi.ac.uk/biomodels-main/BIOMD" ++ x) : urls xs
			else " " : urls xs


linkbtns [] [] = []
linkbtns (x:xs) (y:ys) = (linkButtonNewWithLabel y x) : linkbtns xs ys


meh [] [] _ = return()
meh (x:xs) (y:ys) bx =
			do
				uri <- linkButtonNewWithLabel y x
				boxPackStart bx uri PackNatural 0
				set bx [ boxChildPacking uri := PackNatural]

		 		set uri [ widgetHeightRequest := 24]
				meh xs ys bx





matchSpecPieces [] _ = ["",""]
matchSpecPieces (p:pieces) spec =
				if (head(p) == spec) then
					tail(p)
				else matchSpecPieces pieces spec

matchSpecsPieces [] [] = []
matchSpecsPieces _ [] = []
matchSpecsPieces [] _ = []
matchSpecsPieces xs (y:ys) = ((matchSpecPieces xs y) : matchSpecsPieces xs ys)



onlinecomments [] = []
onlinecomments (l:ls) =
		if ((isInfixOf "-- BioModel" l) || (isInfixOf "-- BioModels" l))
			then (( words l ) : onlinecomments ls)
		else onlinecomments ls

multiDel [] _ = []
multiDel (w:ws) ls = (delete w ls ) ++ multiDel ws (delete w ls )






pros :: [Definition] -> [Process]
pros [] = []
pros ((SpeciesDef _ _ _):xs) = pros xs
pros ((ProcessDef _ proc):xs) = proc : (pros xs)

--fullpros :: [Definition] -> [Process]
fullpros [] = []
fullpros ((SpeciesDef _ _ _):xs) = fullpros xs
fullpros ((ProcessDef p proc):xs) = (ProcessDef p proc) : (fullpros xs)

specs :: [Definition] -> [Species]
specs [] = []
specs ((ProcessDef _ _):xs) = (specs xs)
specs ((SpeciesDef _ _ s ):xs) = s: (specs xs)


specNames :: [Definition] -> [Name]
specNames [] = []
specNames ((ProcessDef _ _):xs) = (specNames xs)
specNames ((SpeciesDef s _ _):xs) = s: (specNames xs)

proNames :: [Definition] -> [String]
proNames [] = []
proNames ((SpeciesDef _ _ _):xs) = proNames xs
proNames ((ProcessDef  proc _):xs) = proc : (proNames xs)

spectablenames :: [(String,[Double])] -> [String]
spectablenames [] = []
spectablenames (( str, _ ):xs) = str:(spectablenames xs)

spectableval :: [(String,[Double])] -> [[Double]]
spectableval [] = []
spectableval ((_, vals):xs) = vals: (spectableval xs)

spectableN :: (String,[Double]) -> String
spectableN ( str, _) = str

spectableV :: (String,[Double]) -> [Double]
spectableV (_,vals) = vals




openUrlBySystemTool :: String -> IO ()
openUrlBySystemTool url = do
  			infos <- appInfoGetAllForType "text/html"
			case infos of
			    [] -> return ()
			    xs -> appInfoLaunchUris (head xs) [url] Nothing



odeClicked :: GUI ->  IO ()
odeClicked gui  =
    do  Just tds <- get (parse gui) modelDef 				;
	if (tds /= []) then do
		let ps = pros tds;
		let pns = map T.pack $ proNames tds
		let p = ps!!(0);
		let ss = speciesInProc p
		let sns = map T.pack $ specNames tds
		let mts = processMTS tds p
		let dpdt = dPdt tds mts p
		--buff2 <- textViewGetBuffer $ textview2 gui;
		box <- containerGetChildren (hbox10 gui)
		mapM_ (containerRemove (hbox10 gui)) box
		list <- comboBoxSetModelText (simProComboBox gui)
		addPros (simProComboBox gui) pns (length pns) (length pns)
		list3 <- comboBoxSetModelText (phaseSpecies1Combobox gui)
		addSpecs (phaseSpecies1Combobox gui) sns (length sns) (length sns)
		list4 <- comboBoxSetModelText (phaseSpecies2Combobox gui)
		addSpecs (phaseSpecies2Combobox gui) sns (length sns) (length sns)

		list5 <- comboBoxSetModelText (formProBox1 gui)
		addPros (formProBox1 gui) pns (length pns) (length pns)
		list6 <- comboBoxSetModelText (formProBox2 gui)
		addPros (formProBox2 gui) pns (length pns) (length pns)
		list7 <- comboBoxSetModelText (formProBox3 gui)
		addPros (formProBox3 gui) pns (length pns) (length pns)
		list8 <- comboBoxSetModelText (formProBox4 gui)
		addPros (formProBox4 gui) pns (length pns) (length pns)
		list9 <- comboBoxSetModelText (formProBox5 gui)
		addPros (formProBox5 gui) pns (length pns) (length pns)
		list10 <- comboBoxSetModelText (formProBox6 gui)
		addPros (formProBox6 gui) pns (length pns) (length pns)
		list11 <- comboBoxSetModelText (formProBox7 gui)
		addPros (formProBox7 gui) pns (length pns) (length pns)
		list12 <- comboBoxSetModelText (formProBox8 gui)
		addPros (formProBox8 gui) pns (length pns) (length pns)

		let ls = lines(prettyODE tds dpdt )
		let [s,d] = transpose (map (splitOn "===>") ls)

		addTreeview s gui (hbox10 gui) "Equation" 100
		addTreeview d gui (hbox10 gui) "Formula" 200

		widgetShowAll (hbox1 gui)
		labelSetLabel (odelabel gui) "ODE's";
	else do	labelSetLabel (odelabel gui) "ODE's";



--maybeID :: Attr DrawingArea (Maybe (ConnectId DrawingArea))
--maybeID = unsafePerformIO $ objectCreateAttribute



plotClicked :: GUI ->  IO ()
plotClicked gui  =
    do 	Just tds <- get (parse gui) modelDef 				;
	if (tds /= []) then do
		pn <- (comboBoxGetActive (simProComboBox gui))

		if (pn > -1) then do

			let ps = pros tds;
			let pns = proNames tds
			let p = ps!!(pn);
			plotpointstemp <- entryGetText $ simPointsEntry gui;
			plotendtemp <- entryGetText $ simEndEntry gui;
			plotstarttemp <- entryGetText $ simStartEntry gui;
			if ((isInteger plotpointstemp ) && (isInteger plotstarttemp) && ( isInteger plotendtemp)) && (((read plotpointstemp::Int) > 1) && ((read plotstarttemp::Int) > -1) && ((read plotendtemp::Int) > 1) && ((read plotendtemp::Int) > (read plotstarttemp::Int)))
				then do
					let mts = processMTS tds p
					let dpdt = dPdt tds mts p
					let ts = ((read plotpointstemp),((read plotstarttemp),(read plotendtemp)))
					let ts' = timePoints ts
					let solns = solveODE tds p mts dpdt ts
					let file = "graph.out"
					let ss = speciesIn tds dpdt
					let ss' = speciesInProc p
					box <- containerGetChildren (hbox7 gui)
					mapM_ (containerRemove (hbox7 gui)) box
					let sps = (map pretty ss')

					let bools = fillPlot $ length sps

					set (rePlotBtn gui) [ plotList := (Just bools)]
					addChecks gui 0 sps
					let drawArea = (drawingArea1 gui)
					widgetSetSizeRequest drawArea  600 400
					(ww, wh) <- widgetGetSizeRequest drawArea
					plotTimeSeriesFilteredD drawArea ts' solns ss ss'
					widgetShowAll (vbox5 gui)
					on (rePlotBtn gui)  buttonActivated $ filterPlot gui ts' solns ss ss'
					on (plotSaveBtn gui) buttonActivated $ plotSave gui ts' solns ss ss'
					labelSetLabel (plotlabel gui) "Plot";
				else do labelSetLabel (plotlabel gui) "error invalid entries";
		else labelSetLabel (plotlabel gui) "error invalid entries";
	else do	labelSetLabel (plotlabel gui) "error no model loaded";


plotList :: Attr Button (Maybe ([Bool]))
plotList = unsafePerformIO $ objectCreateAttribute


addChecks gui _ [] = return ()
addChecks gui n (l:ls) = do
				c <- checkButtonNewWithLabel l
				boxPackStart (hbox7 gui) c PackNatural 0
 				set c [toggleButtonActive := True]
				on c toggled $ flipplots gui n
				addChecks gui (n+1) ls

fillPlot n = replicate n True


flipplots gui n = do
			Just bs <- get (rePlotBtn gui) plotList 				;
			let c = swapbl bs n 0
			set (rePlotBtn gui) [ plotList :=  ( Just c)]


swapbl [] _ _ = []
swapbl (b:bs) n i = if (n == i) then
				(not b) : swapbl bs n (i+1)
		    else b : swapbl bs n (i+1)

filterPlot gui ts' solns ss ss'  = do
			Just a <- get (rePlotBtn gui) plotList
			let blss = zip a ss'
			let fs =  specFill blss
			(ww, wh) <- widgetGetSizeRequest (drawingArea1 gui)
			plotTimeSeriesFilteredD (drawingArea1 gui) ts' solns ss fs
			widgetShowAll (vbox5 gui)


specFill [] = []
specFill ((x,y):xs) = if (x == True) then
				y: specFill xs
		    else specFill xs

modelCheckClicked :: GUI ->  IO ()
modelCheckClicked gui  =
	do 	Just tds <- get (parse gui) modelDef 				;


		checkForm (formEntry1 gui) (formLbl1 gui) (formProBox1 gui) tds
		checkForm (formEntry2 gui) (formLbl2 gui) (formProBox2 gui) tds
		checkForm (formEntry3 gui) (formLbl3 gui) (formProBox3 gui) tds
		checkForm (formEntry4 gui) (formLbl4 gui) (formProBox4 gui) tds
		checkForm (formEntry5 gui) (formLbl5 gui) (formProBox5 gui) tds
		checkForm (formEntry6 gui) (formLbl6 gui) (formProBox6 gui) tds
		checkForm (formEntry7 gui) (formLbl7 gui) (formProBox7 gui) tds
		checkForm (formEntry7 gui) (formLbl7 gui) (formProBox7 gui) tds

checkForm formEntry formLbl proBox tds = do
				pn <- comboBoxGetActive proBox
				if (pn > -1) then do
					let ps = pros tds;
					let p = ps!!(pn);
					form <- entryGetText formEntry
					case parseFormula (form) of
		                       		Left err -> do
							let text  = "error"
							labelSetText formLbl text
		                      		Right f  -> do
							let f' = reconcileSpecs tds f
		                                  	let text = modelCheckSig tds
		                                                 solveODE
		                                                 Nothing
		                                                 p
		                              			 (500,(0,100))
		                                                 f' --}
							labelSetText formLbl (show text)
				else return()






tableClicked :: GUI ->  IO ()
tableClicked gui  =
    do
	Just tds <- get (parse gui) modelDef 				;
	if (tds /= []) then do
		pn <- (comboBoxGetActive (simProComboBox gui))

		if (pn > -1) then do

			let ps = pros tds;
			let pns = proNames tds
			let p = ps!!(pn);
			tabpointstemp <- entryGetText $ simPointsEntry gui;
			tabendtemp <- entryGetText $ simEndEntry gui;
			tabstarttemp <- entryGetText $ simStartEntry gui;
			if ((isInteger tabpointstemp ) && (isInteger tabstarttemp) && ( isInteger tabendtemp)) && (((read tabpointstemp::Float) > 1) && ((read tabstarttemp::Float) > -1) && ((read tabendtemp::Float) > 1) && ((read tabendtemp::Float) > (read tabstarttemp::Float)))
				then do

					let mts = processMTS tds p
					let dpdt = dPdt tds mts p
					let ts = ((read tabpointstemp),((read tabstarttemp),(read tabendtemp)))
					let ts' = timePoints ts
					let solns = solveODE tds p mts dpdt ts
					let file = "graph.out"
					let ss = speciesIn tds dpdt
					let ss' = speciesInProc p
					let meh = (zip (map pretty ss) (map LA.toList (LA.toColumns solns)))
					let treeviews = createTrees (length(meh))
					box <- containerGetChildren (hbox2 gui)
					mapM_ (containerRemove (hbox2 gui)) box
					fillTable gui meh treeviews (length(meh))
					widgetShowAll (hbox2 gui)
					labelSetLabel (tablabel gui) "Table";
				else do	labelSetLabel (tablabel gui) "error invalid entries";
			else do	labelSetLabel (tablabel gui) "error invalid entries";
	else do	labelSetLabel (tablabel gui) "error no model loaded";

csvSaveClicked :: FilePath -> GUI ->  IO ()
csvSaveClicked filepath gui  =
 do
	Just tds <- get (parse gui) modelDef 				;
	if (tds /= []) then do
		pn <- (comboBoxGetActive (simProComboBox gui))

		if (pn > -1) then do

			let ps = pros tds;
			let pns = proNames tds
			let p = ps!!(pn);

			tabpointstemp <- entryGetText $ simPointsEntry gui;
			tabendtemp <- entryGetText $ simEndEntry gui;
			tabstarttemp <- entryGetText $ simStartEntry gui;
			if ((isInteger tabpointstemp ) && (isInteger tabstarttemp) && ( isInteger tabendtemp)) && (((read tabpointstemp::Int) > 1) && ((read tabstarttemp::Int) > -1) && ((read tabendtemp::Int) > 1))
				then do


					let timestep =   (fromInteger $ round $ (((read tabendtemp)-(read tabstarttemp))/((read tabpointstemp)-1)) * (10^2)) / (10.0^^2)
					let mts = processMTS tds p
					let dpdt = dPdt tds mts p
					let ts = ((read tabpointstemp),((read tabstarttemp),(read tabendtemp)))
					let ts' = timePoints ts
					let solns = solveODE tds p mts dpdt ts
					let file = "graph.out"
					let ss = speciesIn tds dpdt
					let ss' = speciesInProc p
					let meh = (zip (map pretty ss) (map LA.toList (LA.toColumns solns)))
					let nms = spectablenames meh
					let vs = spectableval meh
					let vs2 = (transpose vs)
					let vs3 = addTimes vs2 timestep (fromIntegral(read tabstarttemp)) (length(vs2)) (length(vs2))
					let vs4 = map vcsvise vs3
					let csvoutput = "Timepoint," ++ (namecsvise nms) ++ (concat vs4)
					writeFile (filepath ++ ".csv" ) csvoutput
					labelSetLabel (tablabel gui) "Table";
				else do	labelSetLabel (tablabel gui) "error invalid entries";
			else do	labelSetLabel (tablabel gui) "error invalid entries";
	else do	labelSetLabel (tablabel gui) "error no model loaded";


namecsvise :: [String] -> String
namecsvise [] = []
namecsvise (x:xs) =  if ((length(xs)) > 0)
			then ((id x) ++ "," ) ++ namecsvise xs
			else (id x) ++ namecsvise xs ++ "\n"

vcsvise :: [Double] -> String
vcsvise [] = []
vcsvise (x:xs) =  if ((length(xs)) > 0)
			then ((show x) ++ "," ) ++ vcsvise xs
			else (show x) ++ vcsvise xs ++ "\n"


valcsvise ::   Int ->[Double]-> IO()
valcsvise _ [] = return()
valcsvise n xs  = if n > length(xs)
			then return()
			else print (xs!!(n))

addTimes :: [[Double]] -> Double -> Double -> Int -> Int -> [[Double]]
addTimes [] _ _ _ _ = []
addTimes (x:xs) ts strt n i = do
				let temp = ((( (fromIntegral(n - i))*ts) + strt) : x)
				temp : addTimes xs ts strt n (i-1)


repeatnumbers 0 _ vs = return ()
repeatnumbers n i vs =
 do
  mapM_ (valcsvise (i-n)) vs
  repeatnumbers (n-1) i vs



phaseClicked :: GUI ->  IO ()
phaseClicked gui  =
    do 	Just tds <- get (parse gui) modelDef 				;
	if (tds /= []) then do
		pn <- (comboBoxGetActive (simProComboBox gui))
		s1n <- (comboBoxGetActive (phaseSpecies1Combobox gui))
		s2n <- (comboBoxGetActive (phaseSpecies2Combobox gui))
		if ((pn > -1 )&& (s1n > -1) && (s2n > -1) && (s1n /= s2n)) then do
			let ps = pros tds;
			let pns = proNames tds
			let sns = specs tds;
			let p = ps!!(pn);
			let ss1 = sns!!(s1n)
			let ss2 = sns!!(s2n)
			phasepointstemp <- entryGetText $ simPointsEntry gui;
			phaseendtemp <- entryGetText $ simEndEntry gui;
			phasestarttemp <- entryGetText $ simStartEntry gui;
			if ((read phasepointstemp) > 1 )then do
				let mts = processMTS tds p
				let dpdt = dPdt tds mts p
				let ts = ((read phasepointstemp),((read phasestarttemp),(read phaseendtemp)))
				let ts' = timePoints ts
				let solns = solveODE tds p mts dpdt ts
				let file = "graph.out"
				let ss = speciesIn tds dpdt
				let ss' = speciesInProc p
				let drawArea = (drawingArea2 gui)
				widgetSetSizeRequest drawArea  600 400
				(ww, wh) <- widgetGetSizeRequest drawArea
				phasePlot2D drawArea ts' solns ss ((ss'!!(s1n)), (ss'!!(s2n))) -- 600 400
  				--on (drawArea2 gui) mapSignal $ brapPhase (drawArea2 gui) ts' solns ss ((ss'!!(s1n)), (ss'!!(s2n)))
				(phaseSaveBtn gui) `on` buttonActivated $ phaseSave gui ts' solns ss ((ss'!!(s1n)), (ss'!!(s2n)))
				labelSetLabel (phaselabel gui) "Phase Plot"
			else do	labelSetLabel (phaselabel gui) "error";
		else do	labelSetLabel (tablabel gui) "error invalid entries";
	else do	labelSetLabel (tablabel gui) "error invalid entries";

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False


repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  action
  repeatNTimes (n-1) action

createTrees :: Int -> [ IO TreeView]
createTrees 0 = []
createTrees n =
	do
	let tv = treeViewNew
	tv : createTrees (n-1)



addTableColumn :: GUI -> TreeView -> ListStore String -> (String,[Double]) -> IO()
addTableColumn gui tv store spectable = do
			clear store
			let name = (spectableN spectable)
			let vals = (spectableV spectable)
			let valstr = map show vals
			let tree_view = tv
    			appendor store valstr
	    		column <- treeViewColumnNew
	    		cell <- cellRendererTextNew
	 		cellLayoutPackStart column cell True
	    		cellLayoutSetAttributes column cell store $ \row -> [ cellText := row ]
	 		x <- treeViewAppendColumn tree_view column
			treeViewColumnSetTitle column name
	    		treeViewSetModel tree_view store


addTableColumnwithoutstore :: GUI -> TreeView  -> (String,[Double]) -> IO()
addTableColumnwithoutstore gui tree_view spectable = do
			let name = (spectableN spectable)
			let vals = (spectableV spectable)
			let valstr = map show vals
			store <- listStoreNew []
			appendor store valstr
	    		column <- treeViewColumnNew
			cell <- cellRendererTextNew
			cellLayoutPackStart column cell True
			cellLayoutSetAttributes column cell store $ \row -> [ cellText := row ]
	    		x <- treeViewAppendColumn tree_view column
			treeViewColumnSetTitle column name
	    		treeViewSetModel tree_view store






fillTable :: GUI -> [(String,[Double])] -> [IO TreeView] -> Int -> IO()
fillTable gui [] [] _ = return()
fillTable gui [] _  _= return()
fillTable gui _ [] _ = return()
fillTable gui _ _ 0 = return()
fillTable gui spectable tvs n = do

				tv <- (tvs!!(n-1))
				addTableColumnwithoutstore gui (tv) (spectable!!(n-1))
				boxPackStart (hbox2 gui)  (tv)  PackNatural 0
				boxReorderChild (hbox2 gui) tv  (n-1)
				widgetShowAll (hbox2 gui)
				fillTable gui  spectable tvs (n-1)


addPros cb ps 0 i = return ()
addPros cb ps n i =
		do
		comboBoxAppendText cb ((ps!!(i-n)))
		addPros cb ps (n-1) i


addSpecs cb ss 0 i = return ()
addSpecs cb ss n i =
		do
		comboBoxAppendText cb ((ss!!(i-n)))
		addSpecs cb ss (n-1) i


addTim cb = do
	      comboBoxAppendText cb (T.pack "At Some Point Between")
	      comboBoxAppendText cb (T.pack "At All Points Between")
addVal cb = do
	      comboBoxAppendText cb (T.pack "Species")
	      comboBoxAppendText cb (T.pack "Num")

addOp cb = do
	      comboBoxAppendText cb (T.pack "Relational")
	      comboBoxAppendText cb (T.pack "Arithimetic")

addRel cb = do
		comboBoxAppendText cb (T.pack " Greater Than")
		comboBoxAppendText cb (T.pack " Less Than")
		comboBoxAppendText cb (T.pack " Equals")

addAri cb = do
		comboBoxAppendText cb (T.pack "+")
		comboBoxAppendText cb (T.pack "-")
		comboBoxAppendText cb (T.pack "x")
		comboBoxAppendText cb (T.pack "/")


printStringNTimes y ls  x 0 i = return ()
printStringNTimes y ls  x n i =
		do
		listStoreAppend ls ( show ( (@@>) x ( i-n, y )))
		printStringNTimes y ls x (n-1) i


appendor ls [] = return()
appendor ls ss = mapM_ (listStoreAppend ls) ss

clear ls = listStoreClear ls



connectBtn2Gui :: String -> GUI -> Button -> IO (ConnectId Button)
connectBtn2Gui str gui lBtn = do
			lBtn `on` buttonActivated $ lBtnClicked str

lBtnClicked str = openUrlBySystemTool ("http://www.ebi.ac.uk/biomodels-main/"++ str)
