##
## Makefile
##  
## Made by alexandre amblard
## Login   <amblardratiocination.ps.uci.edu>
##
## Started on  Sun Dec  6 14:47:31 2009 alexandre amblard
## Last update Sun Dec  6 14:47:31 2009 alexandre amblard
## 
##############################
# Complete this to make it ! #
##############################
NAME 	= mkkmask		# Name of executable file
NAMEOMP = mkkmask_omp
NAMEMPI = mkkmask_mpi
NAMEOMPI = mkkmask_ompi
SRC	= mkkmask.c		# List of *.c
INCL  	=		# List of *.h
################
# Optional add #
################
IPATH   = -I.           # path of include file
OBJOPT  = -O2 		# option for obj
EXEOPT  = -O2		# option for exe (-lefence ...)
LPATH   = -L.           # path for librairies ... 

#####################
# Macro Definitions #
#####################
CC 	= gcc
CCMPI 	= mpicc
MAKE 	= make
SHELL	= /bin/sh
OBJS 	= $(SRC:.c=.o)
OBJSOMP = $(SRC:.c=_omp.o) 
OBJSMPI = $(SRC:.c=_mpi.o) 
OBJSOMPI = $(SRC:.c=_ompi.o) 
RM 	= /bin/rm -f 	
COMP	= gzip -9v
UNCOMP	= gzip -df
STRIP	= strip

CFLAGS  = $(OBJOPT) $(IPATH)
LDFLAGS = $(EXEOPT) $(LPATH)

.SUFFIXES: .h.Z .c.Z .h.gz .c.gz .c.z .h.z 

##############################
# Basic Compile Instructions #
##############################

%.o : %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%_omp.o : %.c
	$(CC) -fopenmp -D_OMP_ $(CFLAGS) -c -o $@ $<

%_mpi.o : %.c
	$(CCMPI) -D_MPI_ $(CFLAGS) -c -o $@ $<

%_ompi.o : %.c
	$(CCMPI) -fopenmp -D_OMP_ -D_MPI_ $(CFLAGS) -c -o $@ $<

all:	$(NAME) $(NAMEOMP) $(NAMEMPI) $(NAMEOMPI)

$(NAMEOMPI) : $(OBJSOMPI) $(SRC) $(INCL)
	$(CCMPI) $(OBJSOMPI) $(LDFLAGS) -fopenmp -o $(NAMEOMPI)

$(NAMEMPI) : $(OBJSMPI) $(SRC) $(INCL)
	$(CCMPI) $(OBJSMPI) $(LDFLAGS) -o $(NAMEMPI)

$(NAMEOMP) : $(OBJSOMP) $(SRC) $(INCL)
	$(CC) $(OBJSOMP) $(LDFLAGS) -fopenmp -o $(NAMEOMP)

$(NAME): $(OBJS) $(SRC) $(INCL)  
	$(CC) $(OBJS) $(LDFLAGS) -o $(NAME) 
#	$(STRIP) ./$(NAME) # if you debug ,don't strip ...

depend:
	gcc $(IPATH) -MM $(SRC) 
clean:
	-$(RM) $(NAME) $(NAMEOMP)  $(NAMEMPI) $(NAMEOMPI) $(OBJS) $(OBJSOMP) $(OBJSMPI) $(OBJSOMPI) *~
fclean:
	-$(RM) $(NAME)
comp: clean
	$(COMP) $(INCL) $(SRC)
ucomp: 
	$(UNCOMP) $(SRC) $(INCL)

.c.Z.c .h.Z.h .c.gz.c .h.gz.h .c.z.c .h.z.h :
	 -$(UNCOMP) $<

.c.o:
	$(CC) $(CFLAGS) -c $< 
################
# Dependencies #
################
