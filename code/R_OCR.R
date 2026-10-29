#Install OCR packages
install.packages(c(
  "pdftools",
  "magick",
  "tesseract",
  "officer"
))

#Load OCR packages
library(pdftools)
library(magick)
library(tesseract)
library(openxlsx)
library(officer)
library(parallel)

#Create function for standard OCR process. 
OCR_function <- function(filename){
  
  #Record number of pages
  page_count <- pdf_info(filename)$pages
  
  ocr_results <- vector(
    "list",
    page_count
  )
  
  #Loop through all pages in document. 
  for(i in 1:page_count){
    
    cat(
      "Processing page",
      i,
      "of",
      page_count,
      "\n"
    )
    #Convert page to image. 
    img <- pdf_render_page(
      filename,
      page = i,
      dpi = 300
    )
    
    img <- image_read(img)
    #Perform OCR process. 
    text <- ocr(
      img,
      engine = tesseract("eng")
    )
    #Store text results into list. 
    ocr_results[[i]] <- text
    
    rm(img)
    gc()
  }
  
  base_name <- tools::file_path_sans_ext(
    basename(filename)
  )
  
  #Create new text file name. 
  txt_name <- paste0(
    "OCR_",
    base_name,
    ".txt"
  )
  #Write OCR results to text file
  writeLines(
    unlist(ocr_results),
    txt_name
  )
  
  #Perform same steps for word document. 
  doc <- read_docx()
  
  for(i in seq_along(ocr_results)){
    
    doc <- body_add_par(
      doc,
      paste("Page", i),
      style = "heading 1"
    )
    
    doc <- body_add_par(
      doc,
      ocr_results[[i]]
    )
  }
  
  docx_name <- paste0(
    "OCR_",
    base_name,
    ".docx"
  )
  
  print(
    doc,
    target = docx_name
  )
  
  cat(
    "\nCreated:\n",
    txt_name,
    "\n",
    docx_name,
    "\n"
  )
}

#Perform OCR Process on 1 file as test. 
suppressWarnings({
  
  OCR_function(
    "..\\github_samples\\sample scanned letter.pdf"
  )
  
})


#Now perform this process for all pdf files inside a directory
pdf_files <- list.files(
  path = "../github_samples",
  pattern = "\\.pdf$",
  full.names = TRUE
)

#Exclude the large research article for now. Will come back to this later. 
pdf_files = pdf_files[2:3]

for(file in pdf_files){
  
  cat(
    "\nProcessing:",
    basename(file),
    "\n"
  )
  suppressWarnings({
  tryCatch(

    
    OCR_function(file),
    
    error = function(e){
      
      cat(
        "ERROR:",
        basename(file),
        "\n"
      )
      
      print(e)
      
    }
    
  )
  })
  
}


#Now returning to the large research article. 
#Create function to perform OCR process. 

process_page <- function(page_num, pdf_file){
  
  library(pdftools)
  library(magick)
  library(tesseract)
  
  img <- pdf_render_page(
    pdf_file,
    page = page_num,
    dpi = 200
  )
  
  img <- image_read(img)
  
  text <- ocr(img)
  
  rm(img)
  
  list(
    page = page_num,
    text = text
  )
}

filename = "../github_samples/CommonOwnership_scanned.pdf"
page_count <- pdf_info(filename)$pages

ncores <- 8

cl <- makeCluster(ncores)

clusterEvalQ(
  cl,
  {
    library(pdftools)
    library(magick)
    library(tesseract)
    
    NULL
  }
)

clusterExport(
  cl,
  c(
    "process_page",
    "filename"
  )
)



results <- parLapply(
  cl,
  1:page_count,
  process_page,
  pdf_file = filename
  
)

stopCluster(cl)

base_name <- tools::file_path_sans_ext(
  basename(filename)
)

all_text <- sapply(
  results,
  function(x) x$text
)

txt_name <- paste0(
  "OCR_",
  base_name,
  ".txt"
)

writeLines(
  all_text,
  txt_name
)

#Perform same steps for word document. 
doc <- read_docx()

for(i in seq_along(results)){
  
  doc <- body_add_par(
    doc,
    paste("Page", i),
    style = "heading 1"
  )
  
  doc <- body_add_par(
    doc,
    results[[i]]$text
  )
}

docx_name <- paste0(
  "OCR_",
  base_name,
  ".docx"
)

print(
  doc,
  target = docx_name
)
