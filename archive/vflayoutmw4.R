vflayoutmw4 <- function( vfcent, vfperi, pwidth = 8.27,
                         pheight = 11.69, margin = 0.1,
                         filename = NULL,
                         ffamily = "serif", sizetxt = 7,
                         sizetxtSmall = 7, ffamilyvf = "serif", pointsize = 4,
                         outerSymbol = "square", outerInch = 0.125,
                         innerSymbol = "square", innerInch = outerInch / 2,
                         lengthLines = 0, thicknessLines = 0 ) {

  source( "vfplotmw.r" )

  if( nrow( vfcent ) > 1 ) {
    stop("Error! vfcent cannot have more than 1 rows")
  }
  if( nrow( vfperi ) > 1 ) {
    stop("Error! vfperi cannot have more than 1 rows")
  }
  if( vfcent$id != vfperi$id || vfcent$seye != vfperi$seye ) {
    stop("Error! Mistmatch between visual fields: not from same subject and eye" )
  }

  # open window with A4 page
  if( is.null( filename ) ) {
    device <- options( "device" )
    if( .Platform$OS.type == "unix" ) {
      if( Sys.info()["sysname"] == "Darwin" ) {
        options( device = "quartz" )
        dev.new( width = pwidth, height = pheight, dpi = 85 )
      } else {
        options( device = "x11" )
        dev.new( width = pwidth, height = pheight )
      }
    } else{
      options( device = "windows" )
      dev.new( width = pwidth, height = pheight, rescale = "fixed" )
    }
    options( device = device )
  } else {
    pdf( width = pwidth, height = pheight, file = filename )
  }
  
  # define the margins
  mwidth  <- pwidth  - 2 * margin
  mheight <- pheight - 2 * margin
  
  # create the layout of the printout
  printout <- createviewport( "printout", left = margin, top = margin, height = mheight, width = mwidth )
  
  ######################################################
  # first plot all graphs
  ######################################################
  texteval <- paste( vfperi$tperimetry, "locmap$", vfperi$tpattern, sep = "" )
  locmap   <- eval( parse( text = texteval ) )

  # expand axis to min and max of x and y locations
  if( vfperi$seye == "OD" ) {
    xmin <- min( locmap$xod )
    xmax <- max( locmap$xod )
  } else {
    xmin <- -min( locmap$xod )
    xmax <- -max( locmap$xod )
  }
  ymin <- min( locmap$yod )
  yma
x <- max( locmap$yod )
  # expand by 5% each axis
  xmin <- xmin - ( xmax - xmin ) * 0.025
  xmax <- xmax + ( xmax - xmin ) * 0.025
  ymin <- ymin - ( ymax - ymin ) * 0.025
  ymax <- ymax + ( ymax - ymin ) * 0.025

  # total-deviation plot
  opar <- par( no.readonly = TRUE )
  par( fig = c( 0.225, 0.975, 0.675, 0.975 ) )
  vfplotmw( vfcent, plotType = "td", txtfont = ffamilyvf, pointsize = pointsize,
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          outerSymbol = outerSymbol, innerSymbol = innerSymbol,
          outerInch = outerInch, innerInch = innerInch,
          lengthLines = lengthLines, thicknessLines = thicknessLines )
  opar <- par( new = TRUE )
  vfplotmw( vfperi, plotType = "td", txtfont = ffamilyvf, pointsize = pointsize,
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          outerSymbol = outerSymbol, innerSymbol = innerSymbol,
          outerInch = outerInch, innerInch = innerInch,
          lengthLines = lengthLines, thicknessLines = thicknessLines )
  # raw sensitivities
  opar <- par( new = TRUE )
  par( fig = c( 0.225, 0.975, 0.35, 0.65 ) )
  vfplotmw( vfcent, plotType = "vf", txtfont = ffamilyvf, pointsize = pointsize,
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
       
   outerSymbol = outerSymbol, innerSymbol = innerSymbol,
          outerInch = outerInch, innerInch = innerInch,
          lengthLines = lengthLines, thicknessLines = thicknessLines )
  opar <- par( new = TRUE )
  vfplotmw( vfperi, plotType = "vf", txtfont = ffamilyvf, pointsize = pointsize,
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          outerSymbol = outerSymbol, innerSymbol = innerSymbol,
          outerInch = outerInch, innerInch = innerInch,
          lengthLines = lengthLines, thicknessLines = thicknessLines )
  # pattern-deviation plot
  opar <- par( new = TRUE )
  par( fig = c( 0.225, 0.975, 0.025, 0.325 ) )
  vfplotmw( vfcent, plotType = "pd", txtfont = ffamilyvf, pointsize = pointsize,
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          outerSymbol = outerSymbol, innerSymbol = innerSymbol,
          outerInch = outerInch, innerInch = innerInch,
          lengthLines = lengthLines, thicknessLines = thicknessLines )
  opar <- par( new = TRUE )
  vfplotmw( vfperi, plotType = "pd", txtfont = ffamilyvf, pointsize = pointsize,
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          outerSymbol = outerSymbol, innerSymbol = innerSymbol,
          outerInch = outerInch, innerInch = innerInch,
          lengthLines = lengthLines, thicknessLines = thicknessLines )
  # color-code map
  par( new = TRUE )
  par( fig = c( 0.05, 0.2, 0.01, 0.11 ) )
  if( vfenv$nv$nvname == "nvsapmwcps" ) {
    colormapgraph( ncol = 5, notSeenAsBlack = FALSE, txtfont = ffamilyvf,
                   pointsize = pointsize, outerSymbol = outerSymbol, innerSymbol = innerSymbol,
                   outerInch = 1.25 * outerInch, innerInch = 1.25 * innerInch )
  } else {
    colormapgraph( ncol = 6, txtfont = ffamilyvf,
                   pointsize = pointsize, outerSymbol = outerSymbol, innerSymbol = innerSymbol,
                   outerInch = 1.25 * outerInch, innerInch = 1.25 * innerInch )
  }

  par( opar )
  ######################################################
  # create the text elements in the printouts
  ######################################################
  # main info
  mainInfo         <- createviewport( "mainInfo",   left =  0.00, top =  0.00, width = 4.75, height = 0.40, pheight = mheight, pwidth = mwidth )
  # text for TD, PD, and sensitivity maps
  tdtext           <- createviewport( "tdtext",     left =  8.00, top =  0.00, width = 1.83, height = 0.22, pheight = mheight, pwidth = mwidth )
  senstext         <- createviewport( "senstext",   left =  8.00, top =  3.75, width = 1.83, height = 0.22, pheight = mheight, pwidth = mwidth )
  pdtext           <- createviewport( "pdtext",     left =  8.00, top =  7.50, width = 1.83, height = 0.22, pheight = mheight, pwidth = mwidth )
  # information about normative values and software version
  normvaltxt       <- createviewport( "normvaltxt", left =  6.37, top = 10.89, width = 1.40, height = 0.30, pheight = mheight, pwidth = mwidth )
  # central test information
  typetxtcent      <- createviewport( "typetxtcent",     left =  0.05, top =  0.50, width = 1.40, height = 0.40, pheight = mheight, pwidth = mwidth )
  infotxtcent      <- createviewport( "infotxtcent",     left =  0.05, top =  0.75, width = 2.95, height = 0.40, pheight = mheight, pwidth = mwidth )
  labelreliabcent  <- createviewport( "labelreliabcent", left =  0.05, top =  1.10, width = 1.20, height = 0.65, pheight = mheight, pwidth = mwidth )
  reliabilitycent  <- createviewport( "reliabilitycent", left =  0.90, top =  1.10, width = 0.60, height = 0.65, pheight = mheight, pwidth = mwidth )
  labelcent        <- createviewport( "labelcent",       left =  0.05, top =  1.65, width = 0.50, height = 1.10, pheight = mheight, pwidth = mwidth )
  globalcent       <- createviewport( "globalcent",      left =  0.20, top =  1.65, width = 0.70, height = 1.10, pheight = mheight, pwidth = mwidth )
  pvalcent         <- createviewport( "pvalcent",        left =  1.00, top =  1.65, width = 1.00, height = 1.10, pheight = mheight, pwidth = mwidth )
  labeladdcent     <- createviewport( "labeladdcent",    left =  0.05, top =  2.50, width = 1.00, height = 1.10, pheight = mheight, pwidth = mwidth )
  addinfocent      <- createviewport( "addinfocent",     left =  1.25, top =  2.50, width = 1.00, height = 1.10, pheight = mheight, pwidth = mwidth )
  commentscent     <- createviewport( "commentscent",    left =  0.05, top =  3.30, width = 1.00, height = 1.10, pheight = mheight, pwidth = mwidth )
  # peripheral test information
  typetxtperi      <- createviewport( "typetxtperi",     left =  0.05, top =  3.50, width = 1.40, height = 0.40, pheight = mheight, pwidth = mwidth )
  infotxtperi      <- createviewport( "infotxtperi",     left =  0.05, top =  3.75, width = 1.40, height = 0.40, pheight = mheight, pwidth = mwidth )  
  labelreliabperi  <- createviewport( "labelreliabperi", left =  0.05, top =  4.10, width = 1.20, height = 0.65, pheight = mheight, pwidth = mwidth )
  reliabilityperi  <- createviewport( "reliabilityperi", left =  0.90, top =  4.10, width = 0.60, height = 0.65, pheight = mheight, pwidth = mwidth )
  labelperi        <- createviewport( "labelperi",       left =  0.05, top =  4.60, width = 0.50, height = 1.10, pheight = mheight, pwidth = mwidth )
  globalperi       <- createviewport( "globalperi",      left =  0.20, top =  4.60, width = 0.70, height = 1.10, pheight = mheight, pwidth = mwidth )
  pvalperi         <- createviewport( "pvalperi",        left =  1.00, top =  4.60, width = 1.00, height = 1.10, pheight = mheight, pwidth = mwidth )
  labeladdperi     <- createviewport( "labeladdperi",    left =  0.05, top =  5.45, width = 1.00, height = 1.10, pheight = mheight, pwidth = mwidth )
  addinfoperi      <- createviewport( "addinfoperi",     left =  1.25, top =  5.45, width = 1.00, height = 1.10, pheight = mheight, pwidth = mwidth )
  commentsperi     <- createviewport( "commentsperi",    left =  0.05, top =  6.25, width = 1.00, height = 1.10, pheight = mheight, pwidth = mwidth )

  # create the list and then generate the tree and "push" it
  list <- vpList( mainInfo, tdtext, senstext, pdtext, normvaltxt,
                  typetxtcent, infotxtcent, labelreliabcent, reliabilitycent, labelcent, globalcent, pvalcent, labeladdcent, addinfocent, commentscent,
                  typetxtperi, infotxtperi, labelreliabperi, reliabilityperi, labelperi, globalperi, pvalperi, labeladdperi, addinfoperi, commentsperi )
  tree <- vpTree( printout, list )
  
  pushViewport( tree )
  # map info
  seekViewport( "tdtext" )
  grid.text( "Total deviation", x = 0.0, y = 1.0, just = c( "right", "top" ), gp = gpar( fontfamily = ffamily, fontsize = 1.5 * sizetxt, fontface = "bold" ) )
  seekViewport( "senstext" )
  grid.text( "Sensitivity", x = 0.0, y = 1.0, just = c( "right", "top" ), gp = gpar( fontfamily = ffamily, fontsize = 1.5 * sizetxt, fontface = "bold" ) )
  seekViewport( "pdtext" )
  grid.text( "Pattern deviation", x = 0.0, y = 1.0, just = c( "right", "top" ), gp = gpar( fontfamily = ffamily, fontsize = 1.5 * sizetxt, fontface = "bold" ) )
  
  ######################################################
  # perimetry information
  ######################################################
  seekViewport( "mainInfo" )
  text <- "Static Automated Perimetry. Single field analysis."
  # ID
  text <- paste( text, "Subject ID: ", sep = "\n" )
  text <- paste( text, vfcent$id, ",", sep = "" )
  # age
  text <- paste( text, " age: ", round( vfcent$sage ), ",", sep = "" )
  # eye
  texteye <- paste( "eye:", vfcent$seye, sep = " " )
  if( vfcent$seye == "OD" ) {
    texteye <- paste( texteye, "(right)", sep = " " )
  } else if ( vfcent$seye == "OS" ) {
    texteye <- paste( texteye, "(left)", sep = " " )
  } else {
    texteye <- paste( texteye, "(which?)", sep = " " )
  }
  text <- paste( text, texteye, sep = " " )
  grid.text( text, x = 0.0, y = 1.0, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = 1.5 * sizetxt, fontface = "bold" ) )

  ######################################################
  # Details about printouts
  ######################################################
  # algorithm
  textalgorithm <- "ZEST"

  seekViewport( "typetxtcent" )
  
  if( vfcent$tpattern == "p24d2" ) {
    textpattern <- "Central 24-2, size III"
  } else if( vfcent$tpattern == "p30d2" ) {
    textpattern <- "Central 30-2, size III"
  } else if( vfcent$tpattern == "p10d2" ) {
    textpattern <- "Central 10-2, size III"
  } else if( vfcent$tpattern == "p24d2v" ) {
    textpattern <- "Central 24-2, size V"
  } else if( vfcent$tpattern == "p30d2v" ) {
    textpattern <- "Central 30-2, size V"
  } else if( vfcent$tpattern == "p10d2v" ) {
    textpattern <- "Central 10-2, size V"
  } else if( vfcent$tpattern == "p30d1" ) {
    textpattern <- "Central 30-1, size III"
  } else if( vfcent$tpattern == "p30d1v" ) {
    textpattern <- "Central 30-1, size V"
  } else if( vfcent$tpattern == "pPC26" ) {
    textpattern <- "Central G2, size III"
  } else if( vfcent$tpattern == "pPC26v" ) {
    textpattern <- "Central G2, size V"
  } else if( vfcent$tpattern == "pPC26vi" ) {
    textpattern <- "Central G2, size VI"
  } else {
    textpattern <- "Unknown"
  }

  text <- paste( textpattern, textalgorithm, sep = ", algorithm ")
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ),
             gp = gpar( fontfamily = ffamily, fontsize = 1.2 * sizetxt, fontface = "bold") )

  seekViewport( "typetxtperi" )
  if( vfperi$tpattern == "peripheralv" ) {
    textpattern <- "Peripheral grid, size V"
  } else if( vfperi$tpattern == "peripheralvi" ) {
    textpattern <- "Peripheral grid, size VI"
  } else if( vfperi$tpattern == "pG2_peri" ) {
    textpattern <- "Peripheral G2, size III"
  } else if( vfperi$tpattern == "pG2v_peri" ) {
    textpattern <- "Peripheral G2, size V"
  } else if( vfperi$tpattern == "pG2vi_peri" ) {
    textpattern <- "Peripheral G2, size VI"
  } else {
    textpattern <- "Unknown"
  }

  text <- paste( textpattern, textalgorithm, sep = ", algorithm ")
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ),
             gp = gpar( fontfamily = ffamily, fontsize = 1.2 * sizetxt , fontface = "bold") )

  ######################################################
  # Details about printouts
  ######################################################
  seekViewport( "normvaltxt" )
  
  text <- paste( "norm vals: ", vfenv$nv$nvname, sep = "" )
  text <- paste( text, substr( packageDescription( "visualFields" )$Date, 1, 4 ), sep = "\n" )
  text <- paste( text, "visualFields", packageDescription( "visualFields" )$Version, sep = " " )
  grid.text( text, x = 1.00, y = 0.00, just = c( "right", "bottom" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxtSmall ) )

  ######################################################
  # visual-field central test results
  ######################################################
  # subject and test information central test
  seekViewport( "infotxtcent" )
  
  timetxt <- substr( vfcent$ttime, 1, 5 )
  if( substr( timetxt, 1, 1 ) == "0" ) substr( timetxt, 1, 1 ) <- ""
  text <- paste( "Date:", format( vfcent$tdate, "%m/%d/%Y" ), "at", timetxt, sep = " " )
  # duration and pause of test
  timetxt         <- substr( vfcent$sduration, 4, nchar( vfcent$sduration ) )
  if( timetxt != "59:59" ) {
    if( substr( timetxt, 1, 1 ) == "0" ) substr( timetxt, 1, 1 ) <- ""
    text <- paste( text, paste( "Duration: ", timetxt, sep = " " ), sep = "\n" )
  }
  timetxt         <- substr( vfcent$spause, 4, nchar( vfcent$sduration ) )
  if( timetxt != "59:59" ) {
    if( substr( timetxt, 1, 1 ) == "0" ) substr( timetxt, 1, 1 ) <- ""
    text <- paste( text, paste( ", pause: ", timetxt, sep = "" ), sep = "" )
  }
  text <- paste( text, "", sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )

  # false positives, negatives, and fixation losses
  seekViewport( "labelreliabcent" )
  
  text <- "FP"
  text <- paste( text, "FN", sep = "\n" )
  text <- paste( text, "FL", sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )
  
  seekViewport( "reliabilitycent" )
  
  sfp <- paste( sprintf( "%.1f", round( 1000 * vfcent$sfp ) / 10 ), "%", sep = " " )
  sfn <- paste( sprintf( "%.1f", round( 1000 * vfcent$sfn ) / 10 ), "%", sep = " " )
  sfl <- paste( sprintf( "%.1f", round( 1000 * vfcent$sfl ) / 10 ), "%", sep = " " )
  
  text <- sfp
  text <- paste( text, sfn, sep = "\n" )
  text <- paste( text, sfl, sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "right", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) ) 

  # global indices
  vfs  <- vfstats( vfcent )
  vfsp <- vfstatspmap( vfs )
  vfi  <- vfindex( vfcent )
  vfip <- vfindexpmap( vfi )
  # general-height difference, if the used normative values have one.
  texteval <- paste( "vfenv$nv$", vfcent$tpattern, "_", vfcent$talgorithm, "$nvtdrank$mtdr", sep = "" )
  tdr <- NULL
  tdr <- eval( parse( text = texteval ) )
  if( !is.null( tdr ) ) {
    gh <- ghpostd( tdval( vfcent ) )
    gh <- paste( sprintf( "%.1f", round( 10 * gh ) / 10 ), "dB", sep = " " )
  }
  
  ms  <- paste( sprintf( "%.1f", round( 10 * vfs$msens ) / 10 ), "dB", sep = " " )
  md  <- paste( sprintf( "%.1f", round( 10 * vfs$mtdev ) / 10 ), "dB", sep = " " )
  psd <- paste( sprintf( "%.1f", round( 10 * vfs$spdev ) / 10 ), "dB", sep = " " )
  vfi <- paste( sprintf( "%.1f", round( 10 * vfi$mvfi  ) / 10 ), " %", sep = " " )
  
  seekViewport( "labelcent" )
  
  text <- "MS"
  text <- paste( text, "MD", sep = "\n" )
  text <- paste( text, "PSD", sep = "\n" )
  text <- paste( text, "VFI", sep = "\n" )
  if( !is.null( tdr ) ) {
    text <- paste( text, "GH", sep = "\n" )
  }
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )
  
  seekViewport( "globalcent" )
  
  text <- ms
  text <- paste( text, md, sep = "\n" )
  text <- paste( text, psd, sep = "\n" )
  text <- paste( text, vfi, sep = "\n" )
  if( !is.null( tdr ) ) {
    text <- paste( text, gh, sep = "\n" )
  }
  grid.text( text, x = 1.00, y = 1.00, just = c( "right", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )
  
  seekViewport( "pvalcent" )
  
  text <- ""
  textp <- paste( "(p < ", vfsp$mtdev, " %)", sep = "" )
  text <- paste( text, textp, sep = "\n" )
  textp <- paste( "(p < ", vfsp$spdev, " %)", sep = "" )
  text <- paste( text, textp, sep = "\n" )
  textp <- paste( "(p < ", vfip$mvfi, " %)", sep = "" )
  text <- paste( text, textp, sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )

  seekViewport( "labeladdcent" )
  text <- "Foveal thr:"
  text <- paste( text, "Num presentations:", sep = "\n" )
  text <- paste( text, "Outliers:", sep = "\n" )
  text <- paste( text, "Num mistakes:", sep = "\n" )
  text <- paste( text, "Deleted:", sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )

  seekViewport( "addinfocent" )
  text <- vfcent$fovth
  text <- paste( text, vfcent$np, sep = "\n" )
  text <- paste( text, vfcent$outnp, sep = "\n" )
  text <- paste( text, vfcent$mistakes, sep = "\n" )
  text <- paste( text, vfcent$deleted, sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "right", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )

  seekViewport( "commentscent" )
  text <- as.character( vfcent$comments )
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )

  ######################################################
  # visual-field periphery test results
  ######################################################
  # subject and test information periphery
  seekViewport( "infotxtperi" )
  
  timetxt <- substr( vfperi$ttime, 1, 5 )
  if( substr( timetxt, 1, 1 ) == "0" ) substr( timetxt, 1, 1 ) <- ""
  text <- paste( "Date:", format( vfperi$tdate, "%m/%d/%Y" ), "at", timetxt, sep = " " )
  # duration and pause of test
  timetxt         <- substr( vfperi$sduration, 4, nchar( vfperi$sduration ) )
  if( timetxt != "59:59" ) {
    if( substr( timetxt, 1, 1 ) == "0" ) substr( timetxt, 1, 1 ) <- ""
    text <- paste( text, paste( "Duration: ", timetxt, sep = " " ), sep = "\n" )
  }
  timetxt         <- substr( vfperi$spause, 4, nchar( vfperi$sduration ) )
  if( timetxt != "59:59" ) {
    if( substr( timetxt, 1, 1 ) == "0" ) substr( timetxt, 1, 1 ) <- ""
    text <- paste( text, paste( ", pause: ", timetxt, sep = "" ), sep = "" )
  }
  text <- paste( text, "", sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )

  # false positives, negatives, and fixation losses
  seekViewport( "labelreliabperi" )
  
  text <- "FP"
  text <- paste( text, "FN", sep = "\n" )
  text <- paste( text, "FL", sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )
  
  seekViewport( "reliabilityperi" )
  
  sfp <- paste( sprintf( "%.1f", round( 1000 * vfperi$sfp ) / 10 ), "%", sep = " " )
  sfn <- paste( sprintf( "%.1f", round( 1000 * vfperi$sfn ) / 10 ), "%", sep = " " )
  sfl <- paste( sprintf( "%.1f", round( 1000 * vfperi$sfl ) / 10 ), "%", sep = " " )
  
  text <- sfp
  text <- paste( text, sfn, sep = "\n" )
  text <- paste( text, sfl, sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "right", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) ) 
  
  # global indices
  vfs2  <- vfstats( vfperi )
  vfsp2 <- vfstatspmap( vfs2 )
  vfi2  <- vfindex( vfperi )
  vfip2 <- vfindexpmap( vfi2 )
  # general-height difference, if the used normative values have one.
  texteval <- paste( "vfenv$nv$", vfperi$tpattern, "_", vfperi$talgorithm, "$nvtdrank$mtdr", sep = "" )
  tdr <- NULL
  tdr <- eval( parse( text = texteval ) )
  if( !is.null( tdr ) ) {
    gh <- ghpostd( tdval( vfperi ) )
    gh <- paste( sprintf( "%.1f", round( 10 * gh ) / 10 ), "dB", sep = " " )
  }
  
  ms2  <- paste( sprintf( "%.1f", round( 10 * vfs2$msens ) / 10 ), "dB", sep = " " )
  md2  <- paste( sprintf( "%.1f", round( 10 * vfs2$mtdev ) / 10 ), "dB", sep = " " )
  psd2 <- paste( sprintf( "%.1f", round( 10 * vfs2$spdev ) / 10 ), "dB", sep = " " )
  vfi2 <- paste( sprintf( "%.1f", round( 10 * vfi2$mvfi  ) / 10 ), " %", sep = " " )
  
  seekViewport( "labelperi" )
  
  text <- "MS"
  text <- paste( text, "MD", sep = "\n" )
  text <- paste( text, "PSD", sep = "\n" )
  text <- paste( text, "VFI", sep = "\n" )
  if( !is.null( tdr ) ) {
    text <- paste( text, "GH", sep = "\n" )
  }
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )
  
  seekViewport( "globalperi" )
  
  text <- ms2
  text <- paste( text, md2, sep = "\n" )
  text <- paste( text, psd2, sep = "\n" )
  text <- paste( text, vfi2, sep = "\n" )
  if( !is.null( tdr ) ) {
    text <- paste( text, gh, sep = "\n" )
  }
  grid.text( text, x = 1.00, y = 1.00, just = c( "right", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )
  
  seekViewport( "pvalperi" )
  
  text <- ""
  textp <- paste( "(p < ", vfsp2$mtdev, " %)", sep = "" )
  text <- paste( text, textp, sep = "\n" )
  textp <- paste( "(p < ", vfsp2$spdev, " %)", sep = "" )
  text <- paste( text, textp, sep = "\n" )
  textp <- paste( "(p < ", vfip2$mvfi, " %)", sep = "" )
  text <- paste( text, textp, sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )

  seekViewport( "labeladdperi" )
  text <- "Foveal thr:"
  text <- paste( text, "Num presentations:", sep = "\n" )
  text <- paste( text, "Outliers:", sep = "\n" )
  text <- paste( text, "Num mistakes:", sep = "\n" )
  text <- paste( text, "Deleted:", sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )
  
  seekViewport( "addinfoperi" )
  text <- vfperi$fovth
  text <- paste( text, vfperi$np, sep = "\n" )
  text <- paste( text, vfperi$outnp, sep = "\n" )
  text <- paste( text, vfperi$mistakes, sep = "\n" )
  text <- paste( text, vfperi$deleted, sep = "\n" )
  grid.text( text, x = 0.00, y = 1.00, just = c( "right", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )
  
  seekViewport( "commentsperi" )
  text <- as.character( vfperi$comments )
  grid.text( text, x = 0.00, y = 1.00, just = c( "left", "top" ), gp = gpar( fontfamily = ffamily, fontsize = sizetxt ) )

  # only if in save mode, then set device to off
  if( !is.null( filename ) ) {
    dev.off()
  }
  
}