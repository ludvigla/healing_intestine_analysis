cmyk <- function(C,M,Y,K = 0) {

  C <- C / 100.0
  M <- M / 100.0
  Y <- Y / 100.0
  K <- K / 100.0

  n.c <- (C * (1-K) + K)
  n.m <- (M * (1-K) + K)
  n.y <- (Y * (1-K) + K)

  r.col <- ceiling(255 * (1-n.c))
  g.col <- ceiling(255 * (1-n.m))
  b.col <- ceiling(255 * (1-n.y))

  rgb(red = r.col, green = g.col, blue = b.col, maxColorValue = 255)
}


CMYKPlot <- function (
  object,
  K = 0,
  sampleid = NULL,
  dims = 1:3,
  reduction = "umap.3d"
) {
  um <- se[[reduction]]@cell.embeddings
  if (length(dims) !=3) stop("3 dimensions must be selected")
  signs <- sign(dims)
  um <- um[, abs(dims)]
  um <- t(t(um)*signs)
  if (ncol(um) != 3) stop("Reduction must be 3 dimensions")
  um <- apply(um, 2, scales::rescale, c(0, 100))
  cols <- apply(um, 1, function(x) {
    cmyk(C = x[1], M = x[2], Y = x[3], K = K)
  })
  if (!"Staffli" %in% names(se@tools)) stop("No STaffli object present in Seurat object")
  gg <- GetStaffli(se)@meta.data
  maxy <- max(sapply(GetStaffli(se)@limits, function(x) {x[1]}))
  gg$colors <- cols
  if (!is.null(sampleid)) {
    gg <- subset(gg, sample == paste0(sampleid))
  }
  ggplot(gg, aes(x, maxy - y)) +
    geom_point(color = gg$colors) +
    facet_wrap(~sample) +
    theme_void()
}
