#!/bin/bash

cd /gliders

#Grab new glider files 
rsync -ia localuser@usfgliderserver.marine.usf.edu:/var/opt/gmc/gliders* . > newGliderFiles.txt

#sync up cache files needed to process *bd files
rsync -a localuser@usfgliderserver.marine.usf.edu:/var/opt/sfmc-dataserver/stations/default/dataFiles/cache .

#make a list new *cd file and paths to decode with webb utils 
grep sbd newGliderFiles.txt |cut -d " " -f 2 > newSBDList.txt
grep tbd newGliderFiles.txt |cut -d " " -f 2 > newTBDList.txt
grep mbd newGliderFiles.txt |cut -d " " -f 2 > newMBDList.txt
grep nbd newGliderFiles.txt |cut -d " " -f 2 > newNBDList.txt

#first decode sbds 
while read p; do
 #strip glider name part of path
 gliderName=$(cut -d "/" -f 1-2 <<< $p)
 #grab the filename
 fname=$(cut -d "/" -f 4 <<< $p)
 #remove *bd extention 
 name=$(cut -d "." -f 1 <<< $fname)
 ./bin/dbd2asc $p > /echos/"$gliderName"/flight/"$name".ssv
done < newSBDList.txt


#second decode tbds 
while read p; do
 #strip glider name part of path
 gliderName=$(cut -d "/" -f 1-2 <<< $p)
 #grab the filename
 fname=$(cut -d "/" -f 4 <<< $p)
 #remove *bd extention 
 name=$(cut -d "." -f 1 <<< $fname)
 ./bin/dbd2asc $p > /echos/"$gliderName"/science/"$name".ssv
done < newTBDList.txt


#third decode mbds 
while read p; do
 #strip glider name part of path
 gliderName=$(cut -d "/" -f 1-2 <<< $p)
 #grab the filename
 fname=$(cut -d "/" -f 4 <<< $p)
 #remove *bd extention 
 name=$(cut -d "." -f 1 <<< $fname)
 ./bin/dbd2asc $p > /echos/"$gliderName"/flight/"$name"_mbd.ssv
done < newMBDList.txt


#forth decode nbds 
while read p; do
 #strip glider name part of path
 gliderName=$(cut -d "/" -f 1-2 <<< $p)
 #grab the filename
 fname=$(cut -d "/" -f 4 <<< $p)
 #remove *bd extention 
 name=$(cut -d "." -f 1 <<< $fname)
 ./bin/dbd2asc $p > /echos/"$gliderName"/science/"$name"_nbd.ssv
done < newNBDList.txt


rm newTBDList.txt
rm newSBDList.txt
rm newMBDList.txt
rm newNBDList.txt
rm newGliderFiles.txt
