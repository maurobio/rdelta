//==============================================================================
//
// Project : tFile
//           File management class
//
// Release : 1.00  January 2001
// Author  : Denis ZIEGLER  denis.ziegler@free.fr
//
// File    : tfile.cpp
//
// Portability : C++ ANSI (DOS, Windows, Unix,...)
//
// (C) Copyright 2001 - Denis ZIEGLER
//==============================================================================

/***************************************************************************
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation                                          *
 ***************************************************************************/

#include <stdio.h>
#include <string.h>
#include <iostream>
#include "tfile.h"


//===== Basic class =============================================================

tFile::tFile(const char * _name)
{
  name[127] = 0;
  strncpy(name, _name, 127);
}

const char * tFile::get_name(void)
{
  return name;
}

void tFile::change_name(const char *_name)
{
  strncpy(name, _name, 127);
}

int tFile::create(void)
{
  fs.clear();              // reset previous errors
  fs.open(name, ios::out);
  fs.close();
  return(fs.good());       // return error status
}

int tFile::destroy(void)
{
  return(remove(name)==0);
}

int tFile::reset(void)
{
  return(create());
}

int tFile::exist(void)
{
  fs.clear();
  fs.open(name, ios::in);
  fs.close();
  return(fs.good());
}

int tFile::open(const unsigned access_mode)
{
  ios_base::openmode mod;

  switch (access_mode) {
    case AM_READ :
      mod = ios::in | ios::binary;
      break;
    case AM_RDWRITE :
      mod = ios::in | ios::out | ios::ate | ios::binary;
      break;
    case AM_WRITE :
      mod = ios::out | ios::ate | ios::binary;
      break;
    case AM_APPEND :
      mod = ios::out | ios::app | ios::binary;
      break;
    case AM_WRITERST :
      mod = ios::out | ios::binary;
      break;
    default: return(-1);
  }
  fs.clear();
  fs.open(name, mod);
  return(fs.good());
}

int tFile::close(void)
{
  fs.clear();
  fs.close();
  return(fs.good());
}

int tFile::read(char *dest, const int nb)
{
  fs.clear();
  fs.read(dest, nb);
  return(fs.gcount());
}

int tFile::write(char *source, const int nb)
{
  fs.clear();
  fs.write(source, nb);
  return(fs.good());
}

int tFile::goto_begin_r(void)
{
  fs.clear();
  fs.seekg(0, ios::beg);
  return(fs.good());
}

int tFile::goto_end_r(void)
{
  fs.clear();
  fs.seekg(0, ios::end);
  return(fs.good());
}

int tFile::goto_pos_r(const long n)
{
  fs.clear();
  fs.seekg(n);
  return(fs.good());
}

int tFile::shift_pos_r(const long n)
{
  fs.clear();
  fs.seekg(n, ios::cur);
  return(fs.good());
}

long tFile::cur_pos_r(void)
{
  return(fs.tellg());
}

int tFile::goto_begin_w(void)
{
  fs.clear();
  fs.seekp(0, ios::beg);
  return(fs.good());
}

int tFile::goto_end_w(void)
{
  fs.clear();
  fs.seekp(0, ios::end);
  return(fs.good());
}

int tFile::goto_pos_w(const long n)
{
  fs.clear();
  fs.seekp(n);
  return(fs.good());
}

int tFile::shift_pos_w(const long n)
{
  fs.clear();
  fs.seekp(n, ios::cur);
  return(fs.good());
}

long tFile::cur_pos_w(void)
{
  return(fs.tellp());
}

int tFile::ok(void)
{
  return fs.good();
}

int tFile::eof(void)
{
  return fs.eof();
}

int tFile::error(void)
{
  return (fs.bad() || fs.fail());
}

long tFile::size(void)
{
  long p, rep;

  fs.clear();
  if ((p = cur_pos_r()) < 0)  // save current position
    return -1;                // return if error
  goto_end_r();
  rep = cur_pos_r();
  goto_pos_r(p);              // restore previous position
  if (fs.good())
    return(rep);
  else
    return(-1);
}


//===== Text file ==========================================================

int tTextFile::open(const unsigned access_mode)
{
  ios_base::openmode mod;

  switch (access_mode) {
    case AM_READ :
      mod = ios::in;
      break;
    case AM_RDWRITE :
      mod = ios::in | ios::out | ios::ate;
      break;
    case AM_WRITE :
      mod = ios::out | ios::ate;
      break;
    case AM_APPEND :
      mod = ios::out | ios::app;
      break;
    case AM_WRITERST :
      mod = ios::out;
      break;
    default: return(-1);
  }
  fs.clear();
  fs.open(name, mod);
  return(fs.good());
}

int tTextFile::read_line(char *dest, const int lmax)
{
  fs.clear();
  fs.getline(dest, lmax);
  return(fs.good());
}


int tTextFile::write_line(const char *source)
{
  fs.clear();
  fs << source << endl;
  return(fs.good());
}
