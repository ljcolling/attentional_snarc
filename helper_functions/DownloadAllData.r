# This R script downloads all the data files and puts them in a folder labelled "data"
# The files are organised by node code within "data"
# This only needs to be run once, because it's a bit slow!

require(ReplicationProjectTools)
require(glue)
require(httr)
require(jsonlite)

LabNames <- c("Corballis", "Chen", "Mammarella", "Treccani",
               "Javier", "Lukavský", "Lindemann", "Cipora", "Mieth",
               "Hancock", "Toomarian", "Holmes", "Ocampo", "Goffin",
               "Bryce", "Colling (Szűcs)", "Moeller")

Nodes <- c("yu8hp", "vzt3a", "8gdp3", "ecdmw", "xmyd9",
            "6ea38", "xb6t2", "fkjbz", "t5y2h", "2t9re", "encwv",
            "pqcf4", "kx4w9", "kwg95", "5hgk4", "3s4uh", "nzxwr")


system("mkdir data/unprocessed_data")

GetAllNodeFiles <- function(node) {
  GetFileListFromNode <- function(node) {
  token = 'token_code'# Replace token_code with your OSF token #ReplicationProjectTools::MyOSFToken()
  page = 1
  url = glue('https://api.osf.io/v2/nodes/{node}/files/osfstorage/?page={page}')

  web.content = GET(url, add_headers(Authorization = paste("Bearer", token)))
  #web.content = GET(url)
  web.content.parsed = fromJSON(content(web.content, 'text', encoding = "UTF-8"))

  pagesToGet = ceiling(web.content.parsed$links$meta$total / web.content.parsed$links$meta$per_page)

  GetFilesFromOSF <- function(this.url, token) {
  this.page = GET(this.url, add_headers(Authorization = paste("Bearer", token)))
  #this.page = GET(this.url)
  this.page.parsed = fromJSON(content(this.page, 'text', encoding = "UTF-8"))
  this.names = this.page.parsed$data$attributes$name
  this.links = this.page.parsed$data$links$download

  return(tibble(
  names = this.names,
  links = this.links,
  node = node
  ))

  }

  files = lapply(1:pagesToGet, function(x)
  GetFilesFromOSF(
  glue(
  'https://api.osf.io/v2/nodes/{node}/files/osfstorage/?page={x}'
  ),
  token
  ))

  return(Reduce(function(x, y)
  rbind(x, y), files))
  }

  this.nodeFiles = GetFileListFromNode(node)
  this.nodeFiles = this.nodeFiles[which((this.nodeFiles %>% pull(names) %>% grepl(pattern = ".edf")) == FALSE),]

  dataFolder = file.path(getwd(), "data/unprocessed_data")

  DownloadFileFromNode <- function(name, link, node, dataFolder) {
  token = ReplicationProjectTools::MyOSFToken()
  bin.content <-
  GET(link, add_headers(Authorization = paste("Bearer", token)))
  #GET(link)
  name = tolower(name) # always lower case
  filename = glue('{dataFolder}/{node}/{name}')
  file.obj = file(filename, "wb")
  writeBin(object = content(bin.content, "raw"), con = file.obj)
  close(file.obj)
  msg = glue('{name} downloaded ok from {node}!')
  cat(msg)
  cat("\n")
  return(msg)
  }

  system(paste("mkdir ", paste0(dataFolder, "/", node)))
  pmap(
  list(
  this.nodeFiles$names,
  this.nodeFiles$links,
  this.nodeFiles$node,
  dataFolder
  ),
  DownloadFileFromNode
  )
}


require(foreach)
require(doParallel)
doParallel::registerDoParallel(cores = 8)
foreach(i = 1 : length(Nodes)) %dopar% {
  GetAllNodeFiles(node = Nodes[i])
}
