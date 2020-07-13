add_emojis <- function(yi, vi, type = "txt", cex = 1.5, width = 0.25, height = 0.25) {

   # 'cex' is for the size of the text emojis
   # 'width' and 'height' are for the size of the image emojis
   #
   # values need to be adjusted depending on the number of estimates and the
   # size of the plotting device (too lazy to automate this right now)

   type <- match.arg(type, c("txt", "img"))

   if (type == "img" && !requireNamespace("png", quietly=TRUE))
      stop("Please install the 'png' package to show emoji images.")

   k <- length(yi)

   zi <- yi / sqrt(vi)

   # possible options (1-4)
   emoji <- rep(1, k)
   emoji[zi <= -1.96] <- 2
   emoji[zi >=  1.96] <- 3
   emoji[zi >=    10] <- 4
   emoji[zi <=   -10] <- 4

   if (type == "txt") {

      # corresponding text emojis
      pch <- c("😒","😭","😍","😱")

      points(yi, k:1, pch=19, cex=cex*2, col="white")
      points(yi, k:1, pch=pch[emoji], cex=cex)

   }

   if (type == "img") {

      # corresponding image emojis
      # note: need to put these images under the 'emojis' subdir
      pngs <- list(png::readPNG("emojis/emoji_not_amused.png"),
                   png::readPNG("emojis/emoji_sob.png"),
                   png::readPNG("emojis/emoji_hearts.png"),
                   png::readPNG("emojis/emoji_scream.png"))

      invisible(sapply(1:k, function(i) rasterImage(pngs[[emoji[i]]], dat$yi[i]-width, (k:1)[i]-height, dat$yi[i]+width, (k:1)[i]+height)))

   }

}
