library(rvest)
##NY Times##
#Specifying the url for desired website to be scrapped
url <- 'https://www.nytimes.com/section/politics'

#Reading the HTML code from the website - headlines
webpage <- read_html(url)
headline_data = html_nodes(webpage,'.headline , .headline a')
#headline_data = html_text(headline_data) #convert to readable text
#head(headline_data) #hmmm, I think it could work

#okay, so I can get the urls this way, but idk how to get rid of the other crap
attr_data = html_attrs(headline_data) 
attr_data

urlslist = unlist(attr_data)
urlslist = urlslist[grep("http", urlslist)]


#Let's try blurbs on Politics page
url_nytpoli = 'https://www.nytimes.com/section/politics?action=click&pgtype=Homepage&region=TopBar&module=HPMiniNav&contentCollection=Politics&WT.nav=page'
webpage_nytpoli = read_html(url_nytpoli)
blurb_data = html_nodes(webpage,'.summary')
head(blurb_data) #same here

#dates will be hard, as they are listed like 8hrs ago, 20min ago, etc.

#Bueno, aquí está la página politica de NPR
url2 = 'https://www.npr.org/sections/politics/'
webpage2 = read_html(url2)
headline_data = html_nodes(webpage2,'.title a')
#headline_data = html_text(headline_data)
#head(headline_data) #Very nice. NPR Politics does not show a huge list on its front page

#Los Localizadores Uniformes de Recursos (URLs)
attr_data2 = html_attrs(headline_data) 
attr_data2

urlslist2 = unlist(attr_data2)
urlslist2 = urlslist2[grep("http", urlslist2)]
urlslist2

#Y los blurbs...
blurb_data = html_nodes(webpage2,'.teaser a')
blurb_data = html_text(blurb_data)
head(blurb_data) #some html weirdness here, but I bet we could handle it

#Y ahora, el Fox News
url3 = 'http://www.foxnews.com/politics.html'
webpage3 = read_html(url3)
headline_data = html_nodes(webpage3, '.story- a , .article-list .title a')
headline_data = html_text(headline_data)
head(headline_data) #doesn't look bad, the output that is...

#Fox Blurbs Channel - 'We Blurb, You Decide'
blurb_data = html_nodes(webpage3,'.dek a')
blurb_data = html_text(blurb_data)
head(blurb_data) #How lovely, it works!

#Oh God, it's time to do Breitbart :(
url4 = 'http://www.breitbart.com/'
webpage4 = read_html(url4)
headline_data = html_nodes(webpage4, '.title a , #grid-block-0 span , #BBTrendUL a , #disqus-popularUL a , font')
headline_data = html_text(headline_data)
head(headline_data) #Their headlines often show up as disjointed sentence fragments, but that's how they really are

#Breitbart apparently does not do blurbs on its homepage - probably too much nuance...
