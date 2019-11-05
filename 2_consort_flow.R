# 1_consort_flow.R
# CONSORT flow diagram of numbers of submissions and reviews
# called from within 1_analysis.Rmd
# October 2019
library(diagram)

## function to run same plot for submission and reviewer numbers
# had to do separately because of statistical reviews
flow.plot.submission = function(indata, actual.data){

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
  '- No address data (n=',
  format(
    with(
      indata,
      n.bmj + n.bmjopen - n.missing.address
    ),
    big.mark = ','
  ),
  ')\n',
  '- Transfers between two\njournals (n=',
  format(
    with(
      indata,
      n.missing.address - n.excluded.transfer
    ),
    big.mark = ','
  ),
  ')\n',
  '- Countries with under 100\nresults (n=',
  format(
    with(
      indata,
      n.excluded.transfer - n.post.exclude.100
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
  '- Countries with under 100 results\nwith updated address (n=',
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
1	0.27	0.85	white	square	0.33	0.25
2	0.7	0.48	white	square	0.94	0.28
4	0.27	0.11	white	square	0.27	0.25
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

} # end of function for submissions


# version for reviews with stats reviewer and patient exclusions
flow.plot.review = function(indata, actual.data){
  
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
    '- No address data (n=',
    format(
      with(
        indata,
        n.bmj + n.bmjopen - n.missing.address
      ),
      big.mark = ','
    ),
    ')\n',
    '- Statistical reviews (n=',
    format(
      with(
        indata,
        n.missing.address - n.excluded.stats
      ),
      big.mark = ','
    ),
    ')\n',
    '- Patient reviews (n=',
    format(
      with(
        indata,
        n.excluded.stats - n.excluded.patients
      ),
      big.mark = ','
    ),
    ')\n',
    '- Transfers between two\njournals (n=',
    format(
      with(
        indata,
        n.excluded.patients - n.excluded.transfer
      ),
      big.mark = ','
    ),
    ')\n',
    '- Countries with under 100\nresults (n=',
    format(
      with(
        indata,
        n.excluded.transfer - n.post.exclude.100
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
    '- Countries with under 100 results\nwith updated address (n=',
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
1	0.27	0.85	white	square	0.33	0.25
2	0.7	0.48	white	square	1.05	0.28
4	0.27	0.11	white	square	0.27	0.25
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
  
} # end of function for reviews


# run function for two data sources
sub = flow.plot.submission(indata = submission.numbers, actual.data = submission)
rev = flow.plot.review(indata = reviewer.numbers, actual.data = reviewer)

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
    x1 = 0.415,
    y0 = 0.49,
    length = 0.12
  )
} # end of plotit function for reviews

# 
#postscript('figures/consort.flow.eps', width=5, height=4.5, horiz=F)
tiff(
  'figures/consort.flow.tif',
  width = 10.5,
  height = 5.5,
  units = 'in',
  res = 300,
  compression = 'lzw'
)
layout(matrix(c(1,2), ncol=2), widths=c(1,1), heights=1)
plot.it(sub, header='Submissions')
plot.it(rev, header='Reviews')
dev.off()
layout(1) # restore default layout
# and jpeg
jpeg(
  'figures/consort.flow.jpg',
  width = 10.5,
  height = 5.5,
  units = 'in',
  res = 300,
  quality=100
)
layout(matrix(c(1,2), ncol=2), widths=c(1,1), heights=1)
plot.it(sub, header='Manuscripts')
plot.it(rev, header='Peer reviews')
dev.off()
layout(1) # restore default layout


# percents, plus numerators and denominators
show.percents = FALSE
if(show.percents==TRUE){
 cat('Manuscript exclusions:\n')
 with(submission.numbers, cat('Total = ', n.bmjopen + n.bmj, '\n', sep='') )
 #
 total = submission.numbers$n.bmjopen + submission.numbers$n.bmj
 numerator = total - submission.numbers$n.missing.address
 with(submission.numbers, cat('Missing address = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = submission.numbers$n.missing.address
 numerator = total - submission.numbers$n.excluded.transfer
 with(submission.numbers, cat('Transfer = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = submission.numbers$n.excluded.transfer
 numerator = total - submission.numbers$n.post.exclude.100
 with(submission.numbers, cat('Under 100 = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = submission.numbers$n.post.exclude.100
 numerator = total - submission.numbers$n.after.geomatch
 with(submission.numbers, cat('No geocoded address = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = submission.numbers$n.after.geomatch
 numerator = total - submission.numbers$n.post.exclude.differing.country
 with(submission.numbers, cat('Differing country = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = submission.numbers$n.post.exclude.differing.country
 numerator = total - submission.numbers$n.post.exclude.100.second
 with(submission.numbers, cat('Under 100 = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 
 ##
 cat('\nReviewer exclusions:\n')
 with(reviewer.numbers, cat('Total = ', n.bmjopen + n.bmj, '\n', sep='') )
 #
 total = reviewer.numbers$n.bmjopen + reviewer.numbers$n.bmj
 numerator = total - reviewer.numbers$n.missing.address
 with(reviewer.numbers, cat('Missing address = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = reviewer.numbers$n.missing.address
 numerator = total - reviewer.numbers$n.excluded.stats
 with(reviewer.numbers, cat('Stats reviewer = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = reviewer.numbers$n.excluded.stats
 numerator = total - reviewer.numbers$n.excluded.patients
 with(reviewer.numbers, cat('Patient reviewer = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = reviewer.numbers$n.excluded.patients
 numerator = total - reviewer.numbers$n.excluded.transfer
 with(reviewer.numbers, cat('Transfer = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = reviewer.numbers$n.excluded.transfer
 numerator = total - reviewer.numbers$n.post.exclude.100
 with(reviewer.numbers, cat('Under 100 = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = reviewer.numbers$n.post.exclude.100
 numerator = total - reviewer.numbers$n.after.geomatch
 with(reviewer.numbers, cat('No geocoded address = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = reviewer.numbers$n.after.geomatch
 numerator = total - reviewer.numbers$n.post.exclude.differing.country
 with(reviewer.numbers, cat('Differing country = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 #
 total = reviewer.numbers$n.post.exclude.differing.country
 numerator = total - reviewer.numbers$n.post.exclude.100.second
 with(reviewer.numbers, cat('Under 100 = ', round(100*numerator/total,1), '%, ', numerator, '/', total, '\n', sep='') )
 
}
