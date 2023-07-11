# ulimit set in sudo vi /etc/security/limits.conf
# Cstack_info()["size"]
library(shiny)
options(rgl.useNULL=TRUE)

#' This function checks if required Packages are available or not. If there are any packages are missing
#' then it creats a shiny app with message of missing apps. Otherwise it returns NULL object.
#' @param requiredPackages
#'
#' @return Shiny app object
#' @export
#'
#' @examples
loadPackages <- function(requiredPackages=c("shiny")){
    library(shiny)
    notfoundPkgs <-c()
    for (pkg in requiredPackages) {
        if(!require(pkg, character.only = TRUE)){
            notfoundPkgs<-c(notfoundPkgs, pkg)
        }
    }

    if(length(notfoundPkgs)>0){
        ui <- fluidPage(
            titlePanel("ERROR"),
            h2("One or more required pkgs are missing."),
            h2("Please report this error"),
            h3(paste(toString(notfoundPkgs), ": not found"))
        )

        server <- function(input, output) {
        }

        return (shinyApp(ui, server))
    }else{
        return(NULL)
    }
}

requiredPackages <- c("shiny","rgl","DT","Rvcg","molaR","webshot2","shinyFeedback")
checkValue <- loadPackages(requiredPackages = requiredPackages)

if(!is.null(checkValue))
    return(checkValue)

my_OPC3d <-
    function (OPC_Output_Object, binColors = hsv(h = (seq(10, 290,
                                                          40)/360), s = 0.9, v = 0.85),
              patchOutline = FALSE, outlineColor = "black",
              maskDiscard = FALSE, legend = TRUE, legendScale = 1, legendTextCol = "black",
              legendLineCol = "black", leftOffset = 1, fieldofview = 0,
              fileName = NA, binary = FALSE,toLit=FALSE,shape_alpha=0.9999,seg_alpha=100,
              YView=0)
    {

        plyFile <- OPC_Output_Object$plyFile
        bins <- plyFile$Directional_Bins
        BinCount <- as.numeric(length(unique(plyFile$Directional_Bins)))
        BlackPatch <- NULL
        print(binColors)
        for (i in 1:BinCount) {
            Bin <- which(bins == i)
            bins[Bin] <- binColors[i]
            if (maskDiscard == TRUE) {
                if (OPC_Output_Object$Parameters$Minimum_Area ==
                    0) {
                    PatchList <- unlist(OPC_Output_Object$Patches[i],
                                        recursive = F)
                    SmallPatch <- names(which(lapply(PatchList, length) <
                                                  OPC_Output_Object$Parameters$Minimum_Faces))
                    Discarded <- as.numeric(unlist(PatchList[SmallPatch]))
                    BlackPatch <- c(BlackPatch, Discarded)
                }
                if (OPC_Output_Object$Parameters$Minimum_Area > 0) {
                    AreaList <- as.vector(OPC_Output_Object$Patch_Details[[i]][,
                                                                               2])
                    MinAreaPercentage <- sum(OPC_Output_Object$plyFile$Face_Areas) *
                        OPC_Output_Object$Parameters$Minimum_Area
                    SmallPatchList <- which(AreaList < MinAreaPercentage)
                    Discarded <- as.numeric(unlist(OPC_Output_Object$Patches[[i]][SmallPatchList]))
                }
                BlackPatch <- c(BlackPatch, Discarded)
            }
        }
        colormatrix <- bins
        if (maskDiscard == TRUE) {
            colormatrix[BlackPatch] <- "#000000"
        }
        open3d()
        par3d(windowRect = c(100, 100, 900, 900))
        if (patchOutline == TRUE) {
            for (i in 1:BinCount) {
                Orientation <- OPC_Output_Object$Patches[i]
                PatchCount <- as.numeric(length(Orientation[[1]]))
                for (j in 1:PatchCount) {
                    Patch <- Orientation[[1]][j]
                    Patch <- as.numeric(Patch[[1]])
                    Faces <- t(plyFile$it[, Patch])
                    fnum <- length(Faces[, 1])
                    vorder <- vector("list", fnum)
                    for (i in 1:fnum) {
                        vorder[[i]] <- unlist(sort(Faces[i, ]))
                    }
                    edges <- vector("list", fnum)
                    for (i in 1:fnum) {
                        Ordered <- vorder[[i]]
                        G1 <- Ordered[1]
                        G2 <- Ordered[2]
                        G3 <- Ordered[3]
                        ED1 <- paste(G1, G2, sep = "_")
                        ED2 <- paste(G1, G3, sep = "_")
                        ED3 <- paste(G2, G3, sep = "_")
                        edges[[i]] <- paste(ED1, ED2, ED3, sep = ",")
                    }
                    for (i in 1:fnum) {
                        edges[[i]] <- unlist(strsplit(edges[[i]], ","))
                    }
                    string <- unlist(edges)
                    edgeframe <- data.frame(names = string)
                    UniqueEdge <- aggregate(edgeframe, list(edgeframe$names),
                                            FUN = length)
                    PatchEdge <- subset(UniqueEdge, UniqueEdge$names ==
                                            1)
                    EdgeVerts <- as.numeric(unlist(strsplit(as.character(unlist(PatchEdge$Group.1)),
                                                            "_")))
                    EdgeCoords <- plyFile$vb[1:3, EdgeVerts]
                    segments3d(t(EdgeCoords), color = outlineColor,
                               lwd = 1.25, shininess = 120,alpha= seg_alpha, lit=toLit)
                }
            }
        }

        shade3d(plyFile, meshColor = "faces", color = colormatrix,
                shininess = seg_alpha,lit=toLit,alpha= shape_alpha)
        if (legend == TRUE) {
            if (legendScale <= 0) {
                stop("legendScale must be a positive number")
            }
            if (legendScale > 1.05) {
                warning("legendScale greater than 1.05 will restrict legend visibility")
            }
            Fills <- rep("#FFFFFF", BinCount)
            for (i in 1:BinCount) {
                Fills[i] <- binColors[i]
            }
            molaR:::molaR_bgplot(molaR:::OPC_Legend(binColors = Fills, binSize = BinCount,
                                                    maskDiscard = maskDiscard, size = legendScale, textCol = legendTextCol,
                                                    lineCol = legendLineCol))
        }
        if (leftOffset > 1) {
            warning("Left offset greater than 1 may restrict mesh visibility")
        }
        if (leftOffset < -1) {
            warning("Left offset less than -1 may restrict mesh visibility")
        }
        view3d(fov = fieldofview)
        ZView <- par3d("observer")[3]
        XView <- leftOffset * ZView * 0.055
        observer3d(XView, 0, ZView)
        if (!is.na(fileName)) {
            if (!is.character(fileName)) {
                stop("Enter a name for fileName")
            }
            if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) !=
                ".ply") {
                fileName <- paste(fileName, ".ply", sep = "")
            }
            OutPly <- plyFile
            NewVertList <- plyFile$vb[, plyFile$it[1:length(plyFile$it)]]
            NewNormList <- plyFile$normals[, plyFile$it[1:length(plyFile$it)]]
            NewFaceList <- matrix(1:ncol(NewVertList), nrow = 3)
            colormatrix <- matrix(rep(colormatrix, 3), nrow = 3,
                                  byrow = TRUE)
            NewColorList <- colormatrix[1:length(colormatrix)]
            OutPly$vb <- NewVertList
            OutPly$it <- NewFaceList
            OutPly$normals <- NewNormList
            OutPly$material$color <- NewColorList
            return(OutPly)
            # if (binary == FALSE) {
            #     FileText <- readLines(con = paste(getwd(), "/", fileName,
            #                                       sep = ""), warn = F)
            #     NewCom <- paste("comment OPC plot generated in molaR",
            #                     packageVersion("molaR"), "for", R.version.string)
            #     NewCom <- unlist(strsplit(NewCom, split = "\n"))
            #     NewOut <- c(FileText[1:3], NewCom, FileText[(4):length(FileText)])
            #     writeLines(NewOut, con = paste(getwd(), "/", fileName,
            #                                    sep = ""))
            # }
        }
    }

# Define UI
molarUi <- shinyUI(
    pageWithSidebar(
        headerPanel('shinyMolaR extension'),
        sidebarPanel(
            shinyFeedback::useShinyFeedback(),
            p(paste("Max available stack size:",Cstack_info()["size"])),
            fileInput('target_upload', 'Choose ply file to upload',
                      multiple = FALSE,
                      accept = c('.ply')),
            sliderInput("slider1", h3("Front view"),
                        min = -90, max = 90, value = 0,step = 5),
            sliderInput("fov", h3("Field of view"),
                        min = -90, max = 90, value = 0,step = 5),
            downloadButton("downloadData", "Download OPC Ply")

        ),
        mainPanel(
            p("Download the image with right click"),
            h4("Total patch count"),textOutput("text"),
            # rglwidgetOutput("rglDNE", width = "512px", height = "512px"),
            rglwidgetOutput("rglOPC", width = "512px", height = "512px")
        )
    )
)

# Define server logic
molarServer <- shinyServer(function(input, output) {

    df_products_upload <- reactive({
        inFile <- req(input$target_upload)
        print(input$target_upload)
        if (is.null(inFile))
            return(NULL)

        Tooth <-  Rvcg::vcgPlyRead(file = inFile$datapath)
        shinyFeedback::feedbackWarning("target_upload",
                                       dim(vcgBary(Tooth))[1]>10000,
                                       paste("Total number of faces",
                                             dim(vcgBary(Tooth))[1],
                                             "\n(Should be less than 10000)"))
        Tooth$name <- inFile$name
        return(Tooth)
    })

    get_OPC <- reactive({
        Tooth <- df_products_upload()
        id <-
            showNotification(
                type = "message",
                "Calculating OPC...",
                duration = NULL,
                closeButton = FALSE
            )
        on.exit(removeNotification(id), add = TRUE)
        tryCatch({
            OPC1 <- OPC(Tooth, rotation = 22.5)
            OPC1$name <- Tooth$name
            return(OPC1)
        },
        error = function(e) {
            message('Fatal', e)
            showModal(modalDialog(div(paste(
                e, "Try another data"
            )), easyClose = TRUE))

            return(NULL)
        })
    })

    show_OPC3d <- reactive({
        customBinColor <- c("#f70502", "#ff864d", "#ecf002", "#2dd202", "#3a61ff", "#619aff", "#9fdcff", "#de83ff")
        return(my_OPC3d(get_OPC(),toLit = FALSE,fileName = "yesFile",
                        shape_alpha=0.9999, binColors = customBinColor))
    })


    output$rglOPC<- renderRglwidget({
        # options(rgl.useNULL=TRUE)
        # options(rgl.printRglwidget = TRUE)

        Outply <- show_OPC3d()
        if(!is.null(Outply))
            view3d(theta = 0, phi = input$slider1, fov=input$fov)
        rgl::rglwidget()
    })

    # output$rglDNE<- renderRglwidget({
    #     Tooth <- df_products_upload()
    #     DNE1 <- DNE(Tooth)
    #     DNE3d(DNE1)
    #     rglwidget()
    #     # try(close3d())
    # })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste("OPC_", input$target_upload$name, sep = "")
        },
        content = function(file) {
            Outply <- show_OPC3d()
            print(Outply)
            vcgPlyWrite(mesh = Outply, filename = file, binary = FALSE)
        }
    )

    output$downloadFig <- downloadHandler(
        filename = function() {
            paste("OPC_", input$target_upload$name,".png", sep = "")
        },
        content = function(file) {
            Outply <- show_OPC3d()
            # rgl::rglwidget()
            # rgl.snapshot(filename = file)
            snapshot3d(filename = file,webshot = rgl.useNULL())
        }
    )

    output$text <- renderText({ get_OPC()$Patch_Count[["total patches"]] })

}
)

# Run the application
# shinyApp(ui = ui, server = server)
#' Title
#'
#' @return
#' @export
#' @importFrom shiny shinyApp
#' @examples
shinyMolaR <- function(){
    shinyApp(ui = molarUi, server = molarServer)
}
