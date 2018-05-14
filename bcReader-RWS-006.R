
########################################################################################
#                                                                                      #  
#                                                                                      #  
# version: 0.0.6                                                                       #  
# Realease date: 14/05/2018                                                            #  
# Authors: tzagara@upatras.gr                                                          #
#                                                                                      #        
# This script reads Bitcoin's blockchain files (blkxxxxx.dat files), parses them and   #
# outputs some informations about the blocks and transactions                          #
#                                                                                      #
# NOTE: The aim of this script is just to play around and get a feeling of how         #
# blockchain files are strucrtured                                                     #
#                                                                                      #
# Change log                                                                           #  
# v0.0.6 - 15/05/2018:                                                                 #
#     - fixed issues when reaching end of file                                         #
#     - processes now all .dat files in a directory                                    #
# v0.0.3 - 03/04/2018:                                                                 #
#     - first attempt to add hash table to store transactions.                         #
#     - Experimented with RefClasses: environment and cache stats are now encapsulated #
#       in a RefClass                                                                  #
#                                                                                      #  
########################################################################################






#
# Please make sure you have installed the following R packages
#
library(tictoc) # for timing purposes - TODO: maybe another timing package?
library(PKI) #has raw2hex BUT IS OBSOLETE NOW!
library(openssl) # for sha256()
library(broman) # for strftime()
library(logging) # for logging purposes
library(profvis)


# define constants
# TODO: do we really need these?
SIZEOFINT<-4
SIZEOFCHAR<-1
SIZEOFHEADER<-80



# define error codes
# TODO: do we really need these?
E_INVVARVALUE <- -10

# Make above variables constants i.e. make them immutable (i.e. you'll not be able to change their value in the code below)
lockBinding("SIZEOFINT", globalenv())
lockBinding("SIZEOFCHAR", globalenv())
lockBinding("SIZEOFHEADER", globalenv())
lockBinding("E_INVVARVALUE", globalenv())



################################################################
#
# Some helper functions
# 
# 
#
#
#
################################################################




# h2d
# 
# Converts an number in hexadecimal (base=16) into its decimal representation
#
# @hx: a string (character vector) representing a number in hexadecimal
# quick, but ugly and dirty, and FUCK YOY R!
#
# Return values: the hexadecimal input hx in its decimal representation or error if input contains 
# character that's not valid in hexadecimal
# 
# TODO: As said: this is ugly and inefficient. Optimize me!
#
h2d<-function(hx){
  
  lhx <- tolower(hx)
  l<-nchar(lhx)
  val <- 0
  
  # powers
  p<-0
  parts <- strsplit(lhx, "")[[1]]
  
  #TODO: find a better way
  for( c in l:1 ){
    if (parts[c] == 'a') {
      val<-val + 10*16^p
    } else if (parts[c] == 'b') {
      val<-val + 11*16^p
    } else if (parts[c] == 'c'){
      val<-val + 12*16^p
    }else if (parts[c] == 'd'){
      val<-val + 13*16^p
    } else if (parts[c]=='e'){
      val<-val + 14*16^p
    }else if (parts[c]=='f'){
      val<-val + 15*16^p
    } else if (parts[c] == '0'){
      val<-val + 0*16^p
    } else if (parts[c] == '1'){
      val<-val + 1*16^p
    } else if (parts[c]=='2'){
      val<-val + 2*16^p
    } else if (parts[c]=='3'){
      val<-val + 3*16^p
    } else if (parts[c]=='4'){
      val<-val + 4*16^p
    }else if (parts[c]=='5'){
      val<-val + 5*16^p
    }else if (parts[c]=='6'){
      val<-val + 6*16^p
    }else if (parts[c]=='7'){
      val<-val + 7*16^p
    }else if (parts[c]=='8'){
      val<-val + 8*16^p
    }else if (parts[c]=='9'){
      val<-val + 9*16^p
    } else 
      stop( sprintf("[ERROR] Invalid hexadecimal element [%s] in hex number",parts[c]) )
    
    #next power
    p<- p + 1
  }
  
  return(val)
}







################################################################
#
# Functions for reading binary files
# 
# 
#
#
#
################################################################

#
# readInteger
#
# Read integers from a file.
#
# @f : a connection object i.e. a handle to an open file
# @numIntegers: number of integers to read
#
# Return values: a vector with the specified amount of integers from file f
# TODO: what should happen if less than numIntegers are read
#
readInteger<-function(f, numIntegers) {
  
  intVector <- readBin(f, "integer",  n=numIntegers,  size=SIZEOFINT, signed=TRUE, endian="little")
  return(intVector)
}


#
# readBytes
#  
# Read bytes (raw) from an open file and return the bytes as a vector
#
# @f : a connection object i.e. a handle to an open file
# @numBytes: the number of bytes to read from file f
#
# Return values: vector of raw. If fewer than numBytes bytes or 0 bytes were red, an error is displayed.
#
# TODO: Function stops if the less than the required number of bytes have bee red from the file. This must change!
#
readBytes<-function(f, numBytes){
  
  
  bts <- readBin(f, raw(),  n=numBytes,  size=1)
  
  # Have we reached the end of file?
  # TODO: check if this condition indicates eof
  if (length(bts) == 0){
    #stop( "[ERROR] Zero (0) bytes read from file." )  
    # return an empty vector. That's one fancy way of doing it.
    logwarn("Zero  bytes read",  logger='btc.bcreader')
    return( vector(mode="raw", length=0) )
  }
  
  # Ok we have read something. But if we read less bytes than requested (numBytes) 
  # this also may mean that we reached end of file i.e. nothing more there.
  if (length(bts) != numBytes) {
    #stop( sprintf("BCR:::Error reading [%d] bytes. Got [%d] bytes", as.integer(numBytes), length(bts) ) )
    logwarn("Requested [%d] bytes but actual read [%d] from file", numBytes, length(bts), logger='btc.bcreader')
  }
  
  return(bts)
} 



#
# readVariantInteger 
#
# Reads a variant integer from a file i.e. an integer of variable length bytes
#
# @f: a connection object i.e. a handle to an open file
# 
# Return value: a list containing the integer value - returned as float to prevent overflows (key: int) and 
# the actual bytes read (key: bytes). 
# If an invalid integer is detected, an fatal error is generated i.e. the program stops
# 
# 
# Notes:
# The function reads 1, 3, 5 or 9 bytes from  file f.  
# Initially, it reads 1 byte and depending on the value reads
# the next 2, 4 or 8 next bytes. In particular:
# if first byte is smaller than 0xfd if returns the first bytes
# if first byte is equal to fd it reads returns the next 2 bytes as an integer
# if first byte is equal to fe it reads returns the next 4 bytes as an integer
# if first byte is equal to ff it reads returns the next 8 bytes as an integer
#
# TODO: Optimize this. There are a lot of redundant variables here.
#1) removed rbits<-rawToBits(rawvec) line 241
#2) removed rbits<-rawToBits(rawvec)2 line 273 and replaced 275
readVariantInteger<-function(f){
  
  # Read first byte. This will tell us how many next bytes to read    
  rawvec <-readBytes(f, 1)[1]
  #rbits<-rawToBits(rawvec)
  sz<-sum(2^.subset(0:7, as.logical(rawToBits(rawvec))))
  
  vBytes<-c(rawvec)
  
  ###################################mod1########################################### 
  # TODO: better way than h2d("fd") to avoid function call? i.e. check to see if
  # comparing integers and hexadecimals like this sz < 0xfd is possible in R (it should, but need to make sure). IT WORKS
  
  #changelog 1505 removed function call h2d
  # removed rbits
  
  #######################################################
  if (sz < 0xfd ){
    return( list("int"=sz, "bytes"=vBytes) )
  } else {
    if (sz == 0xfd)  {
      tmpSz <- readBytes(f, 2)
      vBytes<-c(vBytes, tmpSz)
      # Convert 16-bit binary integer into decimal
      nSz<-sum(2^.subset(0:15, as.logical(rawToBits(tmpSz))))
      
      # TODO: Remove the next lines
      #tmpSz <- raw2hex( rev(tmpSz) )
      #tmpSz<-h2d( paste(tmpSz, collapse="") )
      
      return (  list("int"=nSz, "bytes"=vBytes) )
      
    } else if (sz == 0xfe){
      tmpSz <- readBytes(f, 4)
      vBytes<-c(vBytes, tmpSz)
      
      #rbits<-rawToBits( rev(tmpSz))
      # Convert 32-bit binary integer into decimal
      nSz<-sum(2^.subset(0:31, as.logical(rawToBits(rawToBits( rev(tmpSz))))))
      
      # TODO: Remove the next lines
      #tmpSz <- raw2hex( rev(tmpSz) )
      #tmpSz<-h2d( paste(tmpSz, collapse="") )
      
      return (  list("int"=nSz, "bytes"=vBytes) )
      
    } else if (sz == 0xff) {
      tmpSz <- readBytes(f, 8)
      vBytes<-c(vBytes, tmpSz)
      
      #rbits<-rawToBits( rev(tmpSz) )
      # Convert 64-bit binary integer into decimal
      nSz<-sum(2^.subset(0:63, as.logical(rawToBits(rawToBits( rev(tmpSz) )))))
      return (  list("int"=nSz, "bytes"=vBytes) )
    }
  }
  
  logerror("Invalid first byte [%f] for variant integer", sz, logger='btc.bcreader')
  stop( sprintf("[ERROR] Invalid first byte [%f] for variant integer", sz) )
}









#
# This reads ONLY strings from a file. 
# Strings MUST BE NULL TERMINATED! If not, don;t use this function.
# TODO: check/fix me.
readString<-function(f, numStrings, maxChars, debug=FALSE) {
  
  if (debug)
    print( sprintf("readString: calling with paramereters: numStrings=%d, maxChars=%d", numStrings, maxChars))
  
  stringsVector <- c()
  for (i in 1:numStrings) {
    str <- readBin(f, "character", n=1, size=maxChars*SIZEOFCHAR)
    if (debug)
      print( sprintf("readString: Read: %s", str[1]))
    if (nchar(str[1]) != maxChars) {
      rest <-maxChars - nchar(str[1]) - 1
      if (debug)
        print( sprintf("readString: reading garbage of size %d (actual read:%d)", rest, nchar(str[1])))
      
      garbage <- readBin(f, "raw", n=rest, size=1 )
    }
    stringsVector <- c(stringsVector, str)
  }
  
  return(stringsVector)
}





################################################################
#
# Below starts the section with functions that read 
# blkxxxxxx.dat files according to the structure 
# of the blockchain
#
#
#
################################################################







#
# createBCInfo
#
# Create the BlockChainInfo object that contain 2 things:
#
# 1) The database/cache where all extracted transactions are stored (field: txRegistry). This is used to lookup
#    previous transactions. The transaction registry is a R environment that is used a hash table. In that hashtable
#    the transactions are stored as follows: key: hash of transaction value: the entire transaction
# 2) Statistics about the database/cache and some other variables (field: statistics)
#
# The BlockChainInfo if a so-called Reference Class in R.This means it has data fields and methods for
# modifying these fields. For more info on Reference Classes
# see: http://adv-r.had.co.nz/R5.html
#
createBCInfo<-function() {
  
  statClass<-setRefClass("statClass", fields = list(lookupAttempts="integer", # how many times did we lookup a Tx in the cache
                                                    lookupHits="integer",     # how many times we found a tx in the cache i.e. hit
                                                    blockCount = "numeric",   # how many blocks we have read
                                                    invalidBlockCount = "numeric", # how many invalid blocks were encoutered
                                                    totalTxCount="numeric", # total number of transactions encountered
                                                    maxBlockTxCount="integer", # Maximum number of Tx seen in block
                                                    maxTxInCount="integer", # Maximum number of inputs seen in a Tx
                                                    maxTxOutCount="integer",  # MAximum number of outputs seen in a Tx
                                                    maxTxOutAmount = "numeric", # Maximum amount of btc seen in outputs
                                                    coinbaseTxCount = "integer", # Number of coinbase Tx seen
                                                    coinbaseAmount = "numeric" # Total amount of btc of coinbase tx
  ),
  methods=list( 
    newLookup=function() {.self$lookupAttempts<-.self$lookupAttempts + 1L
    },
    newHit=function() {.self$lookupHits<-.self$lookupHits + 1L
    },
    reset=function() {.self$lookupAttempts<-0L
    .self$lookupHits<-0L
    .self$maxBlockTxCount<-0L
    .self$maxTxInCount<-0L
    .self$maxTxOutCount<-0L
    .self$maxTxOutAmount<-0.0
    },
    
    newBlock=function(){
      .self$blockCount<-.self$blockCount + 1
    },
    invalidBlock=function(){
      .self$invalidBlockCount<-.self$invalidBlockCount + 1
    },
    
    newTxs=function(amnt){
      .self$totalTxCount<-.self$totalTxCount + amnt
    },
    
    checkMaxBlockTxCount=function(nmx){
      if (.self$maxBlockTxCount <= nmx)
        .self$maxBlockTxCount<-nmx
    },
    newCoinbaseTx=function(amnt){
      .self$coinbaseTxCount<-.self$coinbaseTxCount + 1L
      .self$coinbaseAmount<-.self$coinbaseAmount + amnt
    }
  ) 
  )
  
  
  BCInfoClass<-setRefClass("BCInfoClass", fields=list(txRegistry="environment", statistics="statClass"),
                           methods=list( 
                             initialize=function(tx, sts){
                               .self$txRegistry<-tx #new.env(hash=TRUE)
                               .self$statistics<-sts #txRegStatsClass2$new(attempts=0L, hits=0L)
                             }
                           )
  )
  
  
  
  BCInfo<-BCInfoClass$new(new.env(hash=TRUE), 
                          statClass$new(lookupAttempts=0L, lookupHits=0L, blockCount=0.0, invalidBlockCount=0.0, totalTxCount=0.0, maxBlockTxCount=0L, maxTxInCount=0L, maxTxOutCount=0L, maxTxOutAmount=0.0,
                                        coinbaseTxCount=0L, coinbaseAmount=0.0 ) 
  )
  
  return(BCInfo)
}








#
# readBlockChainFile
#
# Open a blockchain file (i.e. a blkxxxxxx.dat file) and read it block by block
#
# @blockfile: (string) the full path to the blockchain .dat file
# @maxBlocks: (integer) maximum number of blocks to read. Value -1 means all blocks found in file. Default -1
# @saveBlocksToFile: (boolean) whether or not to save blocks to a .txt file. For debugging purposes only!
# @outFile: if saveBlocksToFile is TRUE in which file to store blocks. Defaults to "blocks.txt"
#
# Return values: nothing (yet!)
#
# TODO: This function is unfinished. Still don't know how to finish it.
#
readBlockChainFile<-function(blockfile, maxBlocks=-1L){
  
  # Open the file  
  bF <- file(blockfile, "rb")
  
  blocks<- c()
  blockCounter<-0.0
  
  
  # Setup the blockchain info object.
  # This is an important object as we use it to lookup transactions and keep some statistics.
  blockChainInfo<-createBCInfo()
  
  #For timing block read times
  blockReadElapsed<-c()
  
  # To infinity and beyond...
  while (TRUE){
    
    aBlock<-list()
    #logdebug("=======================================================", logger='btc.bcreader')
    # Read the block
    
    
    # Start timing: to see how long it took to read/process a block
    tic()
    
    # Read next block
    aBlock<-readBlock(bF)
    
    # Stop timer
    eTime=toc(quiet=TRUE)
    # Add the time elapsed to read the block in a vector to calculate avera, max, mean etc
    blockReadElapsed[blockCounter]<-eTime$toc - eTime$tic
    # reset timer
    tic.clear()
    
    #logdebug("=======================================================", logger='btc.bcreader')
    
    
    if ( length(aBlock) == 0 ) {
      logwarn("Zero block found! Terminating reads", logger='btc.bcreader')
      break
    }
    
    
    #
    # At this point we have read a new block which is in variable aBlock.
    #
    # Next we insvestigate the block and keep some stats 
    #
    
    # Update block count in the statistics field
    blockChainInfo$statistics$newBlock()
    
    # increase number of blocks read
    blockCounter<-blockCounter + 1
    
    
    
    
    
           # Display the block that we just red, to make sure everything is ok.
           loginfo( "**********   NEW BLOCK %d (Height????: %d)  ********************", as.integer(blockCounter), as.integer(blockCounter-1), logger='btc.bcreader' )
           loginfo("Magic number: %s", aBlock$magicNumber, logger='btc.bcreader')
           loginfo("Block hash: %s", aBlock$calcBlockHash, logger='btc.bcreader')
           loginfo("Block size: %d bytes", as.integer(aBlock$blockSize), logger='btc.bcreader')
           loginfo("Block time: %s", aBlock$blockTime, logger='btc.bcreader')
           loginfo("Previous Block hash: %s", aBlock$previousHash, logger='btc.bcreader')
           loginfo("Nonce: %f", h2d(aBlock$nonce), logger='btc.bcreader') 
           loginfo("Transaction count: %d ", as.integer(aBlock$TxCount), logger='btc.bcreader')
           loginfo("Transaction count retrieved: %d ", length(aBlock$transactions), logger='btc.bcreader')
           
           # Update the statistics count of how many Tx were inside the block in the blockchaininfo object.
           blockChainInfo$statistics$newTxs( length(aBlock$transactions) )
           
           #
           # Iterate through all transactions found in the block
           #
           for (i in 1:length(aBlock$transactions) ){
             #print( sprintf("    %d) Tx hash: [%s] Inputs:[%d] Outputs:[%d]", i,aBlock$transactions[[i]]$txHash, aBlock$transactions[[i]]$txInCount, aBlock$transactions[[i]]$txOutCount))
             logdebug("%d) Tx hash: [%s] Inputs:[%d] Outputs:[%d]", i,aBlock$transactions[[i]]$txHash, length( aBlock$transactions[[i]]$txInputs ), length( aBlock$transactions[[i]]$txOutputs ), logger='btc.bcreader')
             
             
             handleTx(aBlock$transactions[[i]], blockChainInfo$txRegistry, blockChainInfo$statistics)
             
             # Add the newly extracted transactions into the registry in order to look them up later, when needed
             # We use the transaction hash as the key and store the entire transaction
             
             blockChainInfo$txRegistry[[aBlock$transactions[[i]]$txHash]] <-aBlock$transactions[[i]]
             
             # We pint out some stats of the registry that contains all encountered transactions
             # This is done just to see how the registry can be managed
             # TODO: not sure how to tackle that issue; need to read documentation
             hE <- env.profile(blockChainInfo$txRegistry)
             logdebug("HashMap Size=%d Count=%d", hE$size, hE$nchains, logger='btc.bcreader')
           }
           #print("*******************************************")
           
           # Here we update some statistics. 
           # check here if the number of transactions found in the block is the largest seen
           blockChainInfo$statistics$checkMaxBlockTxCount(as.integer(aBlock$TxCount) )
           
           
           # Have we reached maximum number of blocks to read? If so, stop.
           # TODO: do we need as.interger() casting? I don't think so.
           if ( as.integer(maxBlocks) > 0 ) {
             if ( blockCounter >= as.integer(maxBlocks) ){
               loginfo("Maximum number of blocks %d seen. Stopping", maxBlocks, logger='btc.bcreader')
               break # Bailout of while loop
             }
           }
           
  } # while
  
  
  # Since we are done, print out some stats and variables just to see what they have
  # and cleanup any resources.
  
  
  # 
  # Print out the hash map. 
  # For debugging purposes only!
  # TODO: remove this in production
  #
  #logdebug("*** Printing HashMap keys", logger='btc.bcreader')
  #for ( h in ls(blockChainInfo$txRegistry) ){
  #      logdebug("HashMap key: [%s]", h, logger='btc.bcreader')
  #}
  
  
  
  # Important! Cleanup, i.e. close the .dat file we opened.
  close(bF)
  
  # Print out some statistics
  loginfo("Stats:\n Total blocks read: [%f]\n Cache attempts [%d]\n Cache hits: [%d]\n Success rate: [%f]\n Maximum Tx/block: [%d]\n Total Txs: [%f]\n Coinbase Tx count:[%d]", 
          blockChainInfo$statistics$blockCount,
          blockChainInfo$statistics$lookupAttempts, 
          blockChainInfo$statistics$lookupHits, 
          (blockChainInfo$statistics$lookupHits/blockChainInfo$statistics$lookupAttempts),
          blockChainInfo$statistics$maxBlockTxCount,
          blockChainInfo$statistics$totalTxCount,
          blockChainInfo$statistics$coinbaseTxCount,
          logger='btc.bcreader')
  
  
  loginfo("*** Max block read time: [%f] at iteration [%f]", max(blockReadElapsed), which(blockReadElapsed==max(blockReadElapsed)), logger='btc.bcreader')
  
  loginfo("*** Average read time per block: [%f] secs", mean(blockReadElapsed), logger='btc.bcreader')
  
  #TODO: return the proper structure containing the requested blocks
  #return(blocks)
}




#
# readBlock
#
# Read an entire block from file
#
# @f: a connection object i.e. a handle to an open file
#
# Return values: the block header and the transactions.
# TODO: some redundant stuff here...needs optimization. Also needs better testing as to whether
# transactions are returned appropriately
#
readBlock<-function(f){
  
  # Initialization - for testing purposes
  blkSize<- -9999
  blockValid<-TRUE
  
  # Read magic number (=4 bytes)
  m<-readBytes(f, 4) 
  
  # Did we reach end of file?
  if ( length(m) == 0 ){
       logwarn("No more blocks present. ")
       return( list() ) #return an empty list
  }
  
  
  # Convert binary number into decimal.
  # That's not useful/needed and we should get rid of it - but anyway 
  #mv<-sum(2^.subset(0:31, as.logical(rawToBits(m))))
  
  # Read size of block - in bytes
  bSzBytes<-readBytes(f, 4)
  bSz<-sum(2^.subset(0:31, as.logical( rawToBits(bSzBytes))))
  
  # There is one block in file blk00000.dat that looks very strange: has size=0, magic number=0 etc.
  # Here we just print our if we have encountered such block.
  # However, we need to read that block.
  # TODO: Take a closer at this block. Why is it there? Is this normal? 
  if (bSz == 0){
    logdebug(sprintf(">>>> Zero byte block! (%d)", blkSize), logger='btc.bcreader')
    blockValid<-FALSE
  }
  
  # Read block header
  blockHeader<-list()
  blockHeader<-readHeader(f)
  
  logdebug("Block hash %s", blockHeader$calcBlockHash, logger='btc.bcreader')
  logdebug("Total of %d transactions", as.integer(blockHeader$TxCount), logger='btc.bcreader')
  
  # Read all transactions and add them to a list
  blockTx<-list()
  for (k in 1:blockHeader$TxCount ){
    logdebug("*********** %d) Start transaction ***********", k, logger='btc.bcreader')
    # NOTE:  index k is important!
    txn<-readTx(f)
    #transactionHash<-sha256( sha256(txn$bytes) )
    
    #logdebug("Transaction BYTES: [%s]", paste(txn$bytes, collapse=""), logger='btc.bcreader')
    
    logdebug("Transaction HASH: [%s]", txn$txHash, logger='btc.bcreader' )
    logdebug("*********** End transaction ***********", logger='btc.bcreader')
    blockTx[[length(blockTx)+1]]<-txn
    #myList[[length(myList)+1]] <- list(sample(1:3))
    logdebug("-- New blockTx. length=%d", length(blockTx), logger='btc.bcreader')
  }
  
  blockHeader[["magicNumber"]] <- paste(rev(m),collapse="")
  blockHeader[["blockSize"]]<-bSz
  blockHeader[["blockValid"]]<-blockValid
  blockHeader[["transactions"]]<-blockTx
  
  # blockHeader<-c(blockHeader, "magicNumber" = paste(rev(m),collapse="") )
  # blockHeader<-c(blockHeader, "blockSize"=bSz)
  # logdebug("Adding transaction list to list. Length=%d", length(blockTx), logger='btc.bcreader')
  # blockHeader<-c(blockHeader, "transactions"=blockTx)
  
  
  return(blockHeader)
  
}


#
# readHeader
#
# Read the header of the block (80 bytes)
#
# @f: a connection object i.e. a handle to an open file
#
# Return values: a list containing the block header. 
#
# This function calculates also the current block's hash using sha256.
#
# TODO: some redudancies here. Optimize/improve it.
#
readHeader<-function(f){
  
  
  blkHeader<- "" # TODO: do we need this? don't know. sigh....
  
  # Headers are always 80 bytes in size. Read an entire chunk of 80 bytes
  # and "break" it apart to get the various parts. It's in memory and hence faster.
  blkHeader<- readBytes(f, 80)
  
  # Calculate the blocks hash. 
  # For this we pass the block header bytes into sha256 resulting in a 256bit number and pass
  # that 256bit number again into sha256. The result will be the block's hash value.
  #
  # NOTE: You may use this hash value to check the block data by comparing the data read from file with  
  # data that blockchain.info maintains for a block. To do this, take blockHash value and paste the hash value
  # into the following URL:
  # https://blockchain.info/block/<paste-block-hash-value-here>
  # You should see the block data as maintained by blockchain.info and compare it with the data this script reads
  #
  blockHash<-sha256( sha256(blkHeader) )
  
  
  # Now, chop off the various parts from the 80 bytes (=header) that we have read.
  # TODO: a lot of redundant variables below. Need to improve/optimize this
  #variable reduction 1505
  # Chop off version
  # Reverse bits because of little-endian format 
  # For more on the endianness discussion see https://el.wikipedia.org/wiki/Endianness
  #vVec <- rev(blkHeader[1:4])
  version<-paste(rev(blkHeader[1:4]), collapse="")
  
  # Chop off previous hash
  # Reverse bits because of little-endian format
  #prVec <- rev(blkHeader[5:36])
  previousHash <- paste(rev(blkHeader[5:36]), collapse="")
  
  # Chop off merkle hash
  # Reverse bits because of little-endian format
  #mrklVec <- rev(blkHeader[37:68])
  merkleRoot <- paste(rev(blkHeader[37:68]), collapse="")
  
  logdebug("Merkle root: [%s]", paste(merkleRoot, collapse=""), logger='btc.bcreader')
  
  # Chop off block timestamp
  # We reverse the timestamp bits because integers are stored in little-endian format
  #tmVec <-rev(blkHeader[69:72])
  
  # Chop off timestamp
  # The block's timestamp (=when it was created) is an integer representing an Unix timestamp (see https://en.wikipedia.org/wiki/Unix_time) 
  # This integer is the time elapsed in seconds from 01-01-1970, a date known as "the Epoch"
  # So here we convert the Unix timestamp integer into a datetime. The block reports the timestamp in GMT, hence we do the same here.
  timeStamp <- as.POSIXct( hex2dec(paste(rev(blkHeader[69:72]), collapse="")), origin="1970-01-01")
  timeStampH <- strftime(timeStamp, "%d/%m/%Y %H:%M:%S", tz="GMT") 
  
  # NOTE: Not the actual difficulty but the difficulty bits representing difficulty
  # To see how to convert that number (difficulty bits) into difficulty, see https://en.bitcoin.it/wiki/Difficulty 
  #diffVect  <- rev(blkHeader[73:76])
  difficulty <- paste(rev(blkHeader[73:76]), collapse="")
  
  
  # Chop off nonce (i.e. the value that miners search for in order to solve the hash puzzle)
  #nonceVec <- rev(blkHeader[77:80])
  nonce <- paste(rev(blkHeader[77:80]), collapse="")
  
  # Number of transactions in this block
  txCount<- -2
  txCountList <- readVariantInteger(f)
  if (is.na(txCountList$int)) {
    txCount<-1 #we do this for the strange block with all zeroes so that we don't go berzerk.
  }else{
    txCount<-as.integer(txCountList$int)
  }
  
  
  
  # Note: please note below that we reverse the blockHash, as it is calculated and displayed in little-endian.
  hdr<-list("calcBlockHash"= paste(rev(blockHash), collapse=""), "version"=version, "previousHash"= previousHash, 
            "merkleRoot"=merkleRoot, 
            "blockTime"=timeStampH, "difficultyBits"=difficulty, "nonce"=nonce, "TxCount"=txCount)
  
  hdr<-c(hdr, "error" = 0)
  
  return(hdr)
  
}



#
# readTx
#
# Read one (1) transaction including all its inputs and outputs
#
# @f: a connection object i.e. a handle to an open file
#
# Return values: a list containing the inputs and outputs of that transaction
#
# TODO: redundant variables. Optimize it.
#
readTx<-function(f){ 
  
  txBytes<-c()
  
  # Transaction version
  txVersionNumberV<-readBytes(f, 4)
  txBytes<-c(txBytes, txVersionNumberV)
  
  # Not needed but anyway
  txVersion<-sum(2^.subset(0:31, as.logical( rawToBits(txVersionNumberV))))
  
  ### Read Tx inputs
  
  # How many inputs does the transaction have?
  txInputCountList<-readVariantInteger(f)
  txInputCount<-as.integer(txInputCountList$int)
  txBytes<-c(txBytes, txInputCountList$bytes)
  logdebug("=== Reading (%d) inputs in transaction", txInputCount, logger='btc.bcreader')
  
  # Iterate through all inputs, read them and put them in
  # a vector.
  inTxs<-list()
  for (p in 1:txInputCount){
    txIn<-readTxInput(f)
    txBytes<-c(txBytes, txIn$bytes)
    inTxs[[length(inTxs)+1]]<-txIn
  }
  
  logdebug("=== Inputs done.", logger='btc.bcreader')
  
  ### Read Tx outputs
  
  # How many outputs does the transaction have?
  txOutCountList<-readVariantInteger(f)
  txOutCount<-as.integer(txOutCountList$int)
  txBytes<-c(txBytes, txOutCountList$bytes)
  logdebug("+++ Reading (%d) OUTPUTS in transaction", txOutCount, logger='btc.bcreader')
  
  # Iterate through all outputs, read them and put them in
  # a vector.
  outTxs<-list()
  for (p in 1:txOutCount){
    txOut<-readTxOutput(f)
    txBytes<-c(txBytes, txOut$bytes)
    outTxs[[length(outTxs)+1]]<-txOut
  }
  
  logdebug("+++ OUTPUTS done.", logger='btc.bcreader')  
  
  # The last piece of the transaction to read is the locktime. Read it and we are done.
  # locktime: lock transactions from being considered valid(?) until you reach a certain
  # amount of blockchain hight or some amount of time. I.e. allows you to make tx valid after
  # some point in time.
  # NOTE: if locktime is < 500000000 then its interpreted as block height. if locktime >500000000 then
  # its interpreted as a UNIX timestamp (epoch).
  # TODO: check the above
  lckTm<-readBytes(f, 4)
  txBytes<-c(txBytes, lckTm)
  
  # Next is unnecessary as of now, but we'll need it later anyway...
  lockTime<-sum(2^.subset(0:31, as.logical( rawToBits(lckTm))))
  
  # Now, calculate hash for the transaction we just read
  cTxHash<-sha256( sha256(txBytes) )
  
  return( list("txInCount"=txInputCount, "txInputs"=inTxs, "txOutCount"=txOutCount, "txOutputs"=outTxs, "txHash"=paste(rev(cTxHash), collapse=""), "bytes"=txBytes) )
}


#
# readTxInput
#
# Read one (1) INPUT of a transaction 
#
# @f: a connection object i.e. a handle to an open file
#
# Return values: a list containing ONE INPUT in a transaction
#
# TODO: redundant variables. Optimize it.
#
readTxInput<-function(f){
  
  inTxBytes<-c()
  
  #Hash of previous transaction
  txPHash<-readBytes(f, 32)
  inTxBytes<-c(inTxBytes, txPHash)
  logdebug(" Previous input Tx: previous hash=%s", paste(rev(txPHash), collapse=""), logger='btc.bcreader')
  
  
  #Specific output in the referenced transaction
  # TODO: check if this number is little- or big-endian.
  outIdx<-readBytes(f, 4)
  
  # Please note the following here: this is a little-endian number 
  # but we convert it to decimal by traversing it from left- to right hence 
  # intepreting it in a little-endian way. That way, we don't need to reverse the bytes.
  outputIndex<-sum(2^.subset(0:31, as.logical(rawToBits( outIdx) )))
  
  logdebug("prev hash: [%s] Output index little-endian:[%f]", paste(rev(txPHash), collapse=""), outputIndex, logger='btc.bcreader')
  inTxBytes<-c(inTxBytes, outIdx)
  
  scriptLenList<-readVariantInteger(f)
  scriptLen<-as.integer(scriptLenList$int)
  inTxBytes<-c(inTxBytes, scriptLenList$bytes)
  logdebug(" Script length=%d", scriptLen, logger='btc.bcreader')
  
  
  #First half of script (ScriptSig)
  # unlocking script: unlocks the previous unspend output!
  unlockScript<-readBytes(f, scriptLen)
  inTxBytes<-c(inTxBytes, unlockScript)
  logdebug(" Unlockscript=%s", paste(unlockScript, collapse=""), logger='btc.bcreader')
  
  # If maxed out (i.e. has max value) then ignore locktime. Otherwise take into
  # consideration locktime 
  # TODO: check if this is really so.
  seqNo<-readBytes(f, 4)
  inTxBytes<-c(inTxBytes, seqNo)
  return(list("txPreviousHash"=paste(rev(txPHash), collapse=""), "txInputOutIdx"=outputIndex, "txInScriptLen"=scriptLen, "txInScript"=unlockScript, "txInSequence"=seqNo, "bytes"=inTxBytes))
}


#
# readTxOutput
#
# Read one (1) OUTPUT of a transaction 
#
# @f: a connection object i.e. a handle to an open file
#
# Return values: a list containing ONE OUTPUT found in a transaction
#
# TODO: redundant variables. Optimize it.
#
readTxOutput<-function(f){
  
  outTxBytes<-c()
  # Amount of transaction, in Satoshis. 
  # Note: 1 bitcoin = 100000000 Satoshis
  txA<-readBytes(f, 8)
  outTxBytes<-c(outTxBytes, txA)
  txAmount<-sum(2^.subset(0:63, as.logical( rawToBits(txA))))
  
  logdebug("Tx output amount: %f", txAmount, logger='btc.bcreader')
  
  outScriptLenList<-readVariantInteger(f)
  outScriptLen<-as.integer(outScriptLenList$int)
  outTxBytes<-c(outTxBytes, outScriptLenList$bytes)
  
  
  #ScriptPubKey
  outScript<-readBytes(f, outScriptLen)
  outTxBytes<-c(outTxBytes, outScript)
  
  # Try to execute script
  # TODO: experimental. Needs to be checked
  execScript(outScript)
  
  #
  # The outscript has to be decoded.
  # Take a look at this
  # https://www.siliconian.com/blog/16-bitcoin-blockchain/22-deconstructing-bitcoin-transactions
  # which might give ideas
  
  
  # Check script. 
  # TODO: this is experimental
  handleOutput( outScript )
  
  
  return(list("txOutAmount"=txAmount, "txOutScriptLength"=outScriptLen, "txOutScript"=outScript, "bytes"=outTxBytes))
}


#
# Experimental: what kind of TX this is
# TODO: prototype; complete-fixme
#
handleOutput<-function(outScript){
  
  logdebug("Checking type of ScriptPubKey (locking script):[%s]", paste(outScript, collapse=""), logger='btc.bcreader')
  if ( outScript[1] == "fe")
    logdebug("TYPE? Pay to Public Key Tx", logger='btc.bcreader')
  
  else if ( outScript[1] == "76" )
    logdebug("TYPE? Pay to Address Tx", logger='btc.bcreader')
  
  else if ( outScript[1] == "a9" )
    logdebug("TYPE? Pay to Script Hash Tx", logger='btc.bcreader')
  
  else  if ( outScript[1] == "6a" )
    logdebug("TYPE? NULL DATA Tx", logger='btc.bcreader')
  else 
    logdebug("TYPE? NON STANDARD OR MULTISIG Tx (%s)", outScript[1], logger='btc.bcreader')
  
}




handleTx<-function(tx, txRecord, txRS){
  
  inputAmount<-0.0
  #iterate through all inputs of this Transactions  
  for (inp in tx$txInputs){
    #list("txPreviousHash"=txHash, "txInputOutIdx"=outIdx, "txInScriptLen"=scriptLen, "txInScript"=unlockScript, "txInSequence"=seqNo, "bytes"=inTxBytes)
    if (inp$txPreviousHash != '0000000000000000000000000000000000000000000000000000000000000000') {
      #signal a new lookup               
      txRS$newLookup()
      
      #search for previous transaction in cache/registry
      pFound<-txRecord[[inp$txPreviousHash]]
      
      
      # did we find previous transaction? 
      if (is.null(pFound) ){
        # no. This means we cannot yet calculate fees.
        logdebug("Previous Tx [%s] NOT FOUND in registry", inp$txPreviousHash, logger='btc.bcreader')
        return()
      } else {
        # yes. Signal that we got a hit in the registry
        txRS$newHit()
        logdebug("Previous Tx [%s] ==FOUND== in registry. Prev Tx Output count: [%d] Input index [%d (%d)].", inp$txPreviousHash, pFound$txOutCount, inp$txInputOutIdx, as.integer(inp$txInputOutIdx) +1, logger='btc.bcreader')
        # ok, now use the output index to locate the exact output of the previous transaction that's 
        # the current input
        spendOutput<-pFound$txOutputs[[ as.integer(inp$txInputOutIdx)+1L]]
        # found. Get the amount.
        spendAmount<-spendOutput$txOutAmount
        logdebug("Previous Tx [%s] ==FOUND== in registry. Prev Tx Output count: [%d] Input index [%d]. Amount [%f]", inp$txPreviousHash, pFound$txOutCount, inp$txInputOutIdx, spendAmount, logger='btc.bcreader')
        # add it to calculate total input amount
        inputAmount<-inputAmount + spendAmount
      }
    } else {
      # looks like a coinbase (i.e. generate coin) transaction
      # TODO: make sure we got conditions right! 
      txRS$newCoinbaseTx(0)
      return()
    }
    
  } #for
  
  
  # At this point we have calculate the total input amount.
  # Calculate now the total output amount.
  outputAmount<-0.0
  for (outp in tx$txOutputs){
    outputAmount<-outputAmount + outp$txOutAmount
  }
  
  # the difference will be the fees
  feeAmount<-inputAmount - outputAmount
  logdebug(">>> Tx [%s] input amount: [%f] output amount [%f]. Fee amount [%f]", tx$txHash, inputAmount, outputAmount, feeAmount, logger='btc.bcreader')
  
}


#
# Experimental: trying some kind of stack based interpreter for input and output scripts
# TODO: prototype; complete-fix me
#
execScript<-function(script){
  
  cpos<-1  
  
  while (TRUE){
    
    if (length(script) <= cpos)
      break
    #Changelog
    decScript<-h2d(script[cpos])
    logdebug("Checking: [%s]", script[cpos], logger='btc.bcreader')
    if ( decScript>=1 &&  decScript<=75 ) {
      logdebug("byte data to push read: [%f]", decScript, logger='btc.bcreader')
      address<-script[cpos+1:(cpos+as.integer(decScript) -1)]
      logdebug(">>>address: [%s]", paste(address, collapse=""), logger='btc.bcreader')
      cpos<-cpos+decScript
      return()
    } else {
      switch( as.character(script[cpos]),
              
              "4c" = {
                #cpos<-cpos+1
                nb<-script[cpos]
                nbytes<-h2d(nb)
                cpos<-cpos+1
                pushData<-script[cpos:(cpos+nbytes-1)]
                cpos<-cpos+nbytes
                return()
              },
              "4d" = {
                nb<-paste(script[cpos:(cpos+1)], collapse="")
                nbytes<-h2d(nb)
                cpos<-cpos + 2
                pushData<-script[cpos:(cpos+nbytes-1)]
                cpos<-cpos+nbytes
                return()
              },
              "76" = {
                cpos<-cpos + 1
              },
              "a9" = {
                cpos <- cpos + 1
                address<-script[cpos:(cpos+20-1)]
                logdebug(">>>P2PKH address: [%s]", paste(address, collapse=""), logger='btc.bcreader')
              },
              {
                cpos<-cpos+1
              }
              
      )
      
      
    }
    
  } #while
  
}





#
#
# Execution/testing of above code starts here.
#
#





#
# First, we configure/prepare logging
#

#
# Configure and reset the root logger.
# Calling these function in that sequence, is important to make all handlers to 
# work properly
basicConfig()
logReset()

#
# Create a new logger that we'll use in our application. 
# We call out logger btc.bcreader. PLEASE USE THIS as argument to call any logdebug, loginfo, logwarn, logerror
# Otherwise, messages will not appear
#
btcL<-getLogger('btc.bcreader')

# We add 2 handlers to logger: one for writing to the console and one for writing to a file
# This essentialy means that everything that is shown in the console is also written into a file!
btcL$addHandler(writeToConsole, logger='btc.bcreader', level='DEBUG')

# TODO: CHANGE file TO POINT TO A FILE IN YOUR SYSTEM!
btcL$addHandler(writeToFile, logger='btc.bcreader', file="C:\\Users\\stathis\\Desktop\\diplwmatikh\\bcReader.log", level='DEBUG')


#Set the debug level
# NOTE: The following debug levels are supported:
# DEBUG, INFO, WARN, ERROR.  Setting it to DEBUG means print 
# all messages. Setting it to INFO means show INFO, WARN and ERROR messages i.e. no DEBUG.
# Setting it to WARN, only WARN and ERROR messages will be shown/logged
debugLevel = 'INFO'
setLevel(debugLevel, container='btc.bcreader')


#
# Logging set up. Start now reading the blockchain files
#



#
#
# The next is a test to check if we can iterate over a set of blockchain files
# and process them.
#
#

#Get all .dat files from blockchain directory
# IMPORTANT: Change path to point to the directory on your system where the .dat files rely
blockchainFiles<-sort(list.files("C:\\Users\\stathis\\Desktop\\diplwmatikh\\Blockchain-files\\", full.names=TRUE, ignore.case=TRUE), decreasing=FALSE) 
 
# Process each .dat file
for (bcFile in blockchainFiles){
  
  loginfo( ">>>> Doing file [%s]", bcFile, logger='btc.bcreader') 
  readBlockChainFile(bcFile, maxBlocks=12L) 
  
}


readBlockChainFile("C:\\Users\\stathis\\Desktop\\diplwmatikh\\Blockchain-files\\blk00001.dat",50)















######################## IGNORE CODE BELOW. ONLY FOR TESTING PURPOSES ########################################

#
#
# The next is just a TEST THAT READS A SINGLE, SPECIFIED BLOCKCHAIN FILE
#
#

readBlockChainFile("C:\\home\\users\\tzag\\MyCode\\BlockChainReader\\bc-sample\\blk00000.dat", maxBlocks=-1L) 









