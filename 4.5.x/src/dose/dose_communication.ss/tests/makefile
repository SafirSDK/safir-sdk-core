#################################################
# makefile to create dose_com_test
#
# LSEN 07-11-29/08-01-21 created
#################################################

# Copy DoseTest to this dir
DSTDIR = /home/lasse/bin

CFLAGS = -D_LINUX 
LFLAGS = -lpthread -lrt
CC = g++

OBJS = DoseTest.o DoseTestEvent.o

# This is where libDoseCom.so is located
DOSECOMLIB = -L../../src/dose_com -ldose_com

#%.o : %.cpp
#	$(CC) -c $(CFLAGS)$< -o $@

# build the shared object

dose_com_test : $(OBJS)
	$(CC) -o dose_com_test $(LFLAGS) $(OBJS) $(DOSECOMLIB)
	cp dose_com_test $(DSTDIR)/.
	echo ######### Copied dose_com_test ##########

#  --- All source files ---

DoseTest.o : DoseTest.cpp
	$(CC) -c $(CFLAGS) -o DoseTest.o DoseTest.cpp 

DoseTestEvent.o : DoseTestEvent.cpp
	$(CC) -c $(CFLAGS) -o DoseTestEvent.o DoseTestEvent.cpp 


