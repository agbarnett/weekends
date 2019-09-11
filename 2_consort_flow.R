# 1_consort_flow.R
# CONSORT flow diagram of numbers of submissions and reviews
# called from within 1_analysis.Rmd
# July 2019
library(diagram)

# function to run same plot for submission and reviewer numbers
flow.plot = function(indata, actual.data){

# labels
l1 = paste(
  'Submitted (n=',
  format(
    with(indata, n.bmj + n.bmjopen),
    big.mark = ','
  ),
  ')\n',
  '- BMJ (n=',
  format(with(indata, n.bmj), big.mark = ','),
  ')\n',
  '- BMJ Open (n=',
  format(with(indata, n.bmjopen), big.mark = ','),
  ')',
  sep = ''
)
l2 = paste(
  'Excluded (n=',
  format(
    with(indata, n.bmj + n.bmjopen) - nrow(actual.data),
    big.mark = ','
  ),
  ')\n',
  '- Countries with under\n100 results (n=',
  format(
    with(
      indata,
      n.bmj + n.bmjopen - n.post.exclude.100
    ),
    big.mark = ','
  ),
  ')\n',
  '- No geocoded address (n=',
                                            format(
                                            with(
                                            indata,
                                            n.post.exclude.100  - n.after.geomatch
                                            ),
                                            big.mark = ','
                                            ),
                                            ')\n',
  '- Different country in geocoded\naddress (n=',
  format(
    with(
      indata,
      n.after.geomatch  - n.post.exclude.differing.country
    ),
    big.mark = ','), ')\n',
  '- Countries with under 100\nresults using updated\naddress (n=',
  format(
    with(
      indata,
      n.post.exclude.differing.country  - n.post.exclude.100.second
    ),
    big.mark = ','), ')\n',
  '- Results at start or end with\nincomplete weeks (n=',
  format(
    with(
      indata,
      n.post.exclude.100.second  - n.after.windows
    ),
    big.mark = ','), ')', sep = '')
# get numbers with holidays
with.holidays = filter(actual.data, holiday=='Yes') %>%
  dplyr::select(country) %>%
  unique()
n.hols = nrow(filter(actual.data, country %in% with.holidays$country))
l3 = paste('Analysed (n=', format(nrow(actual.data), big.mark = ','), ')\n',
           '- With holiday data (n=', format(n.hols, big.mark = ','), ')' , sep =
             '')
labels = c(l1, l2, l3)
n.labels = length(labels)
### make data frame of box chars
frame = read.table(
  sep = '\t',
  stringsAsFactors = F,
  skip = 0,
  header = T,
  text = '
i	x	y	box.col	box.type	box.prop	box.size
1	0.27	0.84	white	square	0.33	0.25
2	0.7	0.49	white	square	0.92	0.27
4	0.27	0.14	white	square	0.27	0.25
'
)
#
pos = as.matrix(subset(frame, select = c(x, y)))
M = matrix(
  nrow = n.labels,
  ncol = n.labels,
  byrow = TRUE,
  data = 0
)
M[3, 1] = "' '"

to.return = list()
to.return$M = M
to.return$pos = pos
to.return$labels = labels
to.return$frame = frame
return(to.return)

} # end of function

# run function for two data sources
sub = flow.plot(indata = submission.numbers, actual.data = submission)
rev = flow.plot(indata = reviewer.numbers, actual.data = reviewer)

# function to repeat plot in Rmarkdown and tiff
plot.it = function(inlist, header, type='tiff') {
  # 
  to.plot = inlist
  if(type == 'rmarkdown'){
    to.plot$frame$box.size = to.plot$frame$box.size * 1 # adjustment for rmarkdown (trial and error)
    to.plot$frame$box.prop = to.plot$frame$box.prop * 1
  }
  par(mai = c(0, 0.04, 0.04, 0.04))
  with(to.plot, 
  plotmat(
    M,
    pos = pos,
    name = labels,
    lwd = 1,
    shadow.size = 0,
    curve = 0,
    box.lwd = 2,
    cex.txt = 1,
    box.size = frame$box.size,
    box.prop = frame$box.prop,
    box.col = frame$box.col,
    box.type = frame$box.type,
    arr.type = 'simple',
    txt.col = 'black'
  ))
  text(0.5, 0.98, header, font=2) # bold
  # extra arrow to excluded
  arrows(
    x0 = 0.27,
    x1 = 0.425,
    y0 = 0.49,
    length = 0.12
  )
} # end of plotit function

# 
#postscript('figures/consort.flow.eps', width=5, height=4.5, horiz=F)
tiff(
  'figures/consort.flow.tif',
  width = 10,
  height = 5,
  units = 'in',
  res = 300,
  compression = 'lzw'
)
layout(matrix(c(1,2), ncol=2), widths=c(1,1), heights=1)
plot.it(sub, header='Submissions')
plot.it(rev, header='Reviews')
dev.off()
layout(1) # restore default layout

