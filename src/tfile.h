//==============================================================================
//
// Project : tFile
//           File management class
//
// Release : 1.00  June 2000
// Author  : Denis ZIEGLER  denis.ziegler@free.fr
//
// File    : tfile.h
//
// Portability : C++ ANSI (DOS, Windows, Unix,...)
//
// (C) Copyright 2000 - Denis ZIEGLER
//==============================================================================

/***************************************************************************
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation                                          *
 ***************************************************************************/

#ifndef TFILE_H
#define TFILE_H

#include <fstream>
using namespace std;


//----- File access mode -----
#define AM_READ     0
#define AM_RDWRITE  1
#define AM_WRITE    2
#define AM_APPEND   3  // always writing at end of file
#define AM_WRITERST 4  // file reset at opening
//   Positioning at end of file when opening, excepted in read access mode.
//   In write-only modes (2,3,4) the file is created if it does not exist.

//----- Maximum line size (default) for text file reading -----
#define LMAXLINE 256


//===== Basic class =============================================================

// Functions return value (unless otherwise mentioned)
//   1=Ok; 0=error

class tFile {
 protected :
   fstream fs;      // file access stream
   char name[128];  // complete file name (with path)
 public :
   tFile(const char *_name);            // constructor
   virtual ~tFile() {}                  // FIX: Add virtual destructor
   //--- File name functions   
   const char * get_name(void);         // return file name
   void change_name(const char *_name); // change file name
   //--- General I/O functions
   int create(void);           // create or recreate the file
   int destroy(void);          // destroy the file on disk
   int reset(void);            // file reset (size=0)
   int exist(void);            // test if the file exists (1=yes; 0=no)
   virtual int open(const unsigned access_mode); // open the file
     // acces_mode : cf MA_ constant
     // return value : 1=Ok; 0=error; -1=invalid access mode
   int close(void); // close the file
   int read(char *dest, const int nb);
     // Read <nb> bytes at the current position. File pointer moves <nb> bytes forward.
     //  arguments : pointer to the destination data, number of bytes to read.
     //  return value : number of bytes read (0=Error)
   int write(char *source, const int nb);
     // Writes <nb> bytes at the current position. File pointer moves <nb> bytes forward.
     //  arguments : pointer to the source data, number of bytes to write.
   //--- Reading pointer movement functions
   int goto_begin_r(void);
     // Moves the reading pointer at begin of file
     // Caution ! Although the reading and writing pointers are separate, with different
     // manipulation functions, movement of one means generally the same movement of the other.
   int goto_end_r(void);
     // Moves the reading pointer at end of file
   int goto_pos_r(const long n);
     // Moves the reading pointer at <n> position
   int shift_pos_r(const long n);
     // Shift the reading pointer <n> bytes forward from the current position
   long cur_pos_r(void);
     // Gives the current reading pointer position
     //  return value : -1=error; other=pointer position (bytes from begin of file)
   //--- Writing pointer movement functions (same as reading)
   int goto_begin_w(void);
   int goto_end_w(void);
   int goto_pos_w(const long n);
   int shift_pos_w(const long n);
   long cur_pos_w(void);
   //--- Error status after the last I/O operation
   int ok(void);     // Check if any error condition has occured
   int eof(void);    // Check if end of file is reached (1=yes 0=no)
   int error(void);  // Check if another error condition has occured
   //--- File size in bytes (-1=error)
   long size(void);
};


//===== Text file ==============================================================

class tTextFile : public tFile {
 public :
   tTextFile(const char * _name) : tFile(_name) { }
   virtual ~tTextFile() {}  // FIX: Add virtual destructor
   virtual int open(const unsigned access_mode);
     // Same syntax as tFile
   int read_line(char *dest, const int lmax = LMAXLINE);
     // Reads one line at the current position. Reading pointer moves on next line.
     //  arguments : pointer to the destination data, 
     //              maximum length of <dest> (char. 0 included)
   int write_line(const char *source);
     // Writes one line at the current position. Writing pointer moves after end of line.
     //  argument : pointer to the source data.
   //--- Note : Text lines delimitation depends on implementation.
   //      LF (0A) on Unix systems
   //      CRLF (0D0A) on DOS/Windows systems.
   //    This differences are automatically taken into consideration at compilation time.
};

#endif