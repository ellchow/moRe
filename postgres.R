source('import.R',chdir=T)

import('utils')

postgis.parse.poly <- function(s) {
  if (is.null(names(s)))
    names(s) <- 1:length(s)
  res <- lapply(strsplit(gsub('[)][)]$', '', gsub('^POLYGON[(][(]', '', s)), '[)],[(]'),
         function(x) {
           z <- lapply(strsplit(x, ','),
                  function(y) {
                    z <- strsplit(y, ' ')

                    named(cbind(csplat(rbind, lapply(z, as.double)), TRUE), c('lon','lat','is.hole'), 'col')
                  })

           z[[1]][,3] <- FALSE
           csplat(rbind, z)
         })

  csplat(rbind, lapply(1:length(s), function(i) cbind(polygon=names(s)[i],as.data.frame(res[[i]])) ))
}

postgis.parse.point <- function(s) {
  if (is.null(names(s)))
    names(s) <- 1:length(s)
  cbind(point=names(s), as.data.frame(named(csplat(rbind,
               lapply(strsplit(gsub('[)]$', '', gsub('^POINT[(]', '', s)), ' '),
                      as.double)),
        c('lon','lat'), 'col')))

}

postgis.parse.linestring <- function(s) {
  if (is.null(names(s)))
    names(s) <- 1:length(s)

  csplat(
      rbind,
      lapply(lzip(names(s), strsplit(gsub('[)]$', '', gsub('^LINESTRING[(]', '', s)), ',')),
         function(x) {
             cbind(
                 line.string = x[[1]],
                 named(as.data.frame(csplat(rbind,lapply(strsplit(x[[2]], ' '), as.double))), c('lon','lat'), 'col')
             )
         }))

}
