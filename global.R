# ccnumber <- "5401840112616563"

tbl_genap <- function(ccnumber){
  x <- 1:nchar(ccnumber)
  
  numberdigit <- NULL
  for(i in 1:length(x)) numberdigit[i] <- substr(x = ccnumber, start = x[i], stop = x[i])
  tbl_digit <- data.frame(x = x, digit = as.numeric(numberdigit), genap = as.numeric(x %% 2 == 1))
  tbl_digit$numerik <- ifelse(tbl_digit$genap == 1, as.integer(tbl_digit$digit/5)+((2*tbl_digit$digit) %% 10), tbl_digit$digit)
  
  total_ <- sum(tbl_digit$numerik)*9
  return(total_)
}

tbl_ganjil <- function(ccnumber){
  x <- 1:nchar(ccnumber)
  
  numberdigit <- NULL
  for(i in 1:length(x)) numberdigit[i] <- substr(x = ccnumber, start = x[i], stop = x[i])
  tbl_digit <- data.frame(x = x, digit = as.numeric(numberdigit), genap = as.numeric(x %% 2 == 1))
  tbl_digit$numerik <- ifelse(tbl_digit$genap == 0, as.integer(tbl_digit$digit/5)+((2*tbl_digit$digit) %% 10), tbl_digit$digit)
  
  total_ <- sum(tbl_digit$numerik)*9
  return(total_)
}

# tbl_genap(ccnumber)
# tbl_ganjil(ccnumber)

jenis_kartu <- data.frame(MasterCard = c(51, 52, 53, 54, 55, NA, NA, NA),
                          Visa = c(4, NA, NA, NA, NA, NA, NA, NA),
                          American_Express = c(3, 37, NA, NA, NA, NA, NA, NA),
                          Diners_Club = c(300, 301, 302, 303, 304, 305, 36, 38),
                          JCB = c(18, 21, 30, 31, 33, 35, NA, NA),
                          BANKCARD = c(56, NA, NA, NA, NA, NA, NA, NA),
                          DISCOVER_CARD = c(6011, 65, 644, NA, NA, NA, NA, NA),
                          enRoute = c(201, 214, NA, NA, NA, NA, NA, NA),
                          Voyager = c(869, NA, NA, NA, NA, NA, NA, NA))

length_valid <- data.frame(MasterCard = 16, #c(14, 15, 16),
                          Visa = 16, #c(13, 16, NA),
                          American_Express = c(15, NA, NA),
                          Diners_Club = c(14, NA, NA),
                          JCB = c(15, 16, NA),
                          BANKCARD = c(14, 15, 16),
                          DISCOVER_CARD = c(16, NA, NA),
                          enRoute = c(15, NA, NA),
                          Voyager = c(15, NA, NA))

cardtype <- function(ccnumber){
  type <- ifelse(nchar(ccnumber) %in% length_valid$MasterCard & substr(ccnumber, 1, 2) %in% jenis_kartu$MasterCard, "MasterCard",
                   ifelse(nchar(ccnumber) %in% length_valid$Visa & substr(ccnumber, 1, 1) %in% jenis_kartu$Visa, "Visa",
                          ifelse(nchar(ccnumber) %in% length_valid$American_Express & substr(ccnumber, 1, 2) %in% jenis_kartu$American_Express, "American Express",
                                 ifelse(nchar(ccnumber) %in% length_valid$Diners_Club & substr(ccnumber, 1, 2) %in% jenis_kartu$Diners_Club, "Diners Club",
                                          ifelse(nchar(ccnumber) %in% length_valid$JCB & substr(ccnumber, 1, 2) %in% jenis_kartu$JCB, "JCB",
                                                 ifelse(nchar(ccnumber) %in% length_valid$BANKCARD & substr(ccnumber, 1, 2) %in% jenis_kartu$BANKCARD, "BANKCARD",
                                                        ifelse(nchar(ccnumber) %in% length_valid$DISCOVER_CARD & substr(ccnumber, 1, 2) %in% jenis_kartu$DISCOVER_CARD, "DISCOVER CARD",
                                                               ifelse(nchar(ccnumber) %in% length_valid$enRoute & substr(ccnumber, 1, 2) %in% jenis_kartu$enRoute, "enRoute",
                                                                      ifelse(nchar(ccnumber) %in% length_valid$Voyager & substr(ccnumber, 1, 2) %in% jenis_kartu$Voyager, "Voyager", "Unregistered")))))))))
  return(type)
}

# cardtype(ccnumber)

validity <- function(ccnumber){
  ifelse(test = nchar(ccnumber) == 16, 
         yes = ifelse(test = tbl_genap(ccnumber) %% 10 == 0 & cardtype(ccnumber) %in% names(jenis_kartu),
                      yes = "VALID!", no = "INVALID!"), 
         no = "Only 16 digits support")
}

# validity(ccnumber)

bank_tipe <- read.table(file = "bank-list.txt", header = TRUE, sep = ",", stringsAsFactor = FALSE)

bank <- function(ccnumber){
  if((cardtype(ccnumber) == "Visa" | cardtype(ccnumber) == "MasterCard") & nrow(bank_tipe[bank_tipe$KODE == substr(ccnumber, 1, 6),]) > 0){
    x <- as.character(bank_tipe[bank_tipe$KODE == substr(ccnumber, 1, 6), 2:3])
    return(x)
  } else {
    x <- "na"
  }
}

# bank(ccnumber)
