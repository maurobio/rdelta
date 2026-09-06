//==============================================================================
//
// Project : FREE DELTA - Software system for processing taxonomic description
//           coded in DELTA (DEscription Language for TAxonomy) format
//
// Module  : tDelta - Classes for manipulating delta files
//
// Release : 0.20 beta - August 2001
// Author  : Denis ZIEGLER  denis.ziegler@free.fr
//
// File    : tdelta.cpp
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
#include <stdlib.h>
//#include <iostream>
#include <cstring>
#include <cstdio>

#include <Rcpp.h>

#include "tdelta.h"


//===== tDeltaFile ========================================================

//----- Reads the next line from the file ---------------------------------
int tDeltaFile::next_line(char *dest, const int lmax)
{
  char *p0, *p1;

  while (1) {
    if (read_line(dest, lmax)) {
      lines_nb++;
      p0 = p1 = dest;
      // deletes blanks and tabs at the begin of the line
      while ((*p1==' ')||(*p1=='\t'))
        p1++;
      while (*p1) {
        *p0 = *p1;
        p0++; p1++;
      }
      *p0 = '\x00';
      if (*dest)
        return 1;  // return ok (next valid line in dest)
    }
    else
      return 0;    // return eof or error
  }  // while
}


//===== tDelta ============================================================

// Constructors
tDelta::tDelta(const char *chars_fname, const char *items_fname, int parse)
{
  chars = new tDeltaCharList(chars_fname, parse);
  items = new tDeltaItemList(items_fname, parse);
  specs = NULL;
}

tDelta::tDelta(const char *chars_fname, const char *items_fname,
               const char *specs_fname, int parse)
{
  chars = new tDeltaCharList(chars_fname, parse);
  items = new tDeltaItemList(items_fname, parse);
  specs = new tDeltaSpecs(specs_fname, chars, items, parse);
}

// Destructor
tDelta::~tDelta(void)
{
  if (specs)
    delete specs;
  if (items)
    delete items;
  if (chars)
    delete chars;
}


//===== tDeltaCharList ====================================================

// Constructors

tDeltaCharList::tDeltaCharList(void)
{
  fchars = NULL;
  nbchars = 0;
  parsed = 0;
}

tDeltaCharList::tDeltaCharList(const char *fname, int parse)
{
  fchars = new tDeltaFile(fname);
  nbchars = 0;
  parsed = 0;
  if (parse)
    parse_characters();
}


// Public member functions


//----- Parse the character file ----------------------------------------------
int tDeltaCharList::parse_characters(void)
{
  char cline[LMAXLINE];
  char *p1;
  int ok, nbstates;

  //--- Test if character file exist ---
  if (!fchars)
    return 0;
  //--- Reset data from previous parsing
  if (parsed) {
    char_list.erase(char_list.begin(), char_list.end());
    directives.erase(directives.begin(), directives.end());
  }
  //--- Open the file ---
  if (!fchars->open(AM_READ)) {
    Rcpp::Rcerr << "Unable to open " << fchars->get_name() << endl;
    return 0;
  }
  ok = 1;
  p1 = NULL;
  //--- Reading loop ---
  while (ok) {
    if ((p1==NULL) || (!*p1)) {
      //--- Extract next line from character file ---
      ok = fchars->next_line(cline);
      if (!ok) {
        //--- End of file or error ---
        if (!fchars->eof()) {
          Rcpp::Rcerr << "Error reading " << fchars->get_name() << endl;
          return 0;
        }
        if (nbchars)   // at least one character is already read
          char_list.push_back(cd);  // Store the last character read
        //retrieve_all();  //debug
        //--- End parsing ---
	parsed = 1;
        return 1;
      }
      p1 = cline;
    }
    //--- Processing the line ---
    if (*p1 == '*')
      //--- Reading a directive ---
      ok = read_directive(cline, p1);
    else 
      if (*p1 == '#') {
        //--- Reading a character ---
        nbstates = 0;
        // Before reading the next character, we store the current one
        if (nbchars) {  // at least one character is already read
          char_list.push_back(cd);
          cd.states.erase(cd.states.begin(), cd.states.end());  // reinit state list
        }
        ok = read_character(cline, p1);
      }
      else
        //--- Reading a character state ---
        ok = read_state(cline, p1, nbstates);
  }
  return 0;
}

//----- Set or change the character list filename -----------------------------
void tDeltaCharList::set_filename(const char *fname, int parse)
{
  if (fchars)
    delete fchars;
  fchars = new tDeltaFile(fname);
  nbchars = 0;
  parsed = 0;
  if (parse)
    parse_characters();
}

//----- Retrieves the character list filename ---------------------------------
const char *tDeltaCharList::get_filename(void)
{
  if (fchars)
    return fchars->get_name();
  else
    return "";
}

//----- Retrieves the number of characters ------------------------------------
int tDeltaCharList::get_chars_nb(void) const
{
  return char_list.size();
}

//----- Set or change the character type --------------------------------------
void tDeltaCharList::set_char_type(int charnum, int chartype)
{
  char_list[charnum-1].char_type = chartype;
}

//----- Retrieves the type of a character -------------------------------------
int tDeltaCharList::get_char_type(int charnum) const
{
  if ((charnum < 1) || (charnum > (int)char_list.size()))
    return 0;
  else
    return char_list[charnum-1].char_type;
}

//----- Retrieves the feature of a character ----------------------------------
string tDeltaCharList::get_char_feature(int charnum) const
{
  if ((charnum < 1) || (charnum > (int)char_list.size()))
    return "";
  else
    return char_list[charnum-1].feature;
}

//----- Retrieves the unit of a character (for numeric characters) ------------
string tDeltaCharList::get_char_unit(int charnum) const
{
  if ((charnum < 1) || (charnum > (int)char_list.size()))
    return "";
  if (char_list[charnum-1].char_type & CT_IN)   // numeric character
    return char_list[charnum-1].unit;
  else
    return "";
}

//----- Retrieves the number of state (for a multistate character) ------------
int tDeltaCharList::get_states_nb(int charnum) const
{
  if ((charnum < 1) || (charnum > (int)char_list.size()))
    return 0;
  if (char_list[charnum-1].char_type & CT_UM)   // multistate character
    return char_list[charnum-1].states.size();
  else
    return 0;
}

//----- Retrieves one state (for a multistate character) ----------------------
string tDeltaCharList::get_state(int charnum, int statenum) const
{
  if ((charnum < 1) || (charnum > (int)char_list.size()))
    return "";
  if ((statenum < 1) || (statenum > (int)char_list[charnum-1].states.size()))
    return "";
  if (char_list[charnum-1].char_type & CT_UM)   // multistate character
    return char_list[charnum-1].states[statenum-1];
  else
    return "";
}

//----- For debuging
void tDeltaCharList::retrieve_all(void)
{
  int i, j;

  Rcpp::Rcout << endl << "CHARACTER FILE : " << get_filename() << endl;
  Rcpp::Rcout << "Directives" << endl;
  Rcpp::Rcout << "----------" << endl;
  for (i=0; i<(int)directives.size(); i++)
    Rcpp::Rcout << directives[i] << endl;
  Rcpp::Rcout << "Character list" << endl;
  Rcpp::Rcout << "--------------" << endl;
  for (i=0; i<(int)char_list.size(); i++) {
    Rcpp::Rcout << "Character " << (i+1) << " : " << char_list[i].feature << endl;
    if (char_list[i].char_type & CT_UM)
      for (j=0; j<(int)char_list[i].states.size(); j++)
        Rcpp::Rcout << "  State " << (j+1) << " : " << char_list[i].states[j] << endl;
    else
      if (char_list[i].char_type & CT_IN)
        Rcpp::Rcout << "  Unit : " << char_list[i].unit << endl;
  }
}


// Private member functions

//----- Reads one directive ---------------------------------------------------
int tDeltaCharList::read_directive(char *cline, char * & p1)
{
  directives.push_back(cline);
  p1 += strlen(cline);  // moves p1 at end of line
  return 1;
}

//----- Reads the feature of a character --------------------------------------
int tDeltaCharList::read_character(char *cline, char * & p1)
{
  char buf[BUFSIZE];
  char *p2;;
  int n, stop;

  //--- Extract and verify the character number ---
  p1++;
  n = atoi(p1);
  if (n != nbchars+1) {
    Rcpp::Rcerr << "Parse error line " << fchars->get_lines_nb() << " : character sequence break" << endl;
    return 0;
  }
  nbchars++;
  //----- Extract feature description -----
  // Skip at begin of character description
  while ((*p1>='0' && *p1<='9') || (*p1=='.') || (*p1==' '))
    p1++;
  p2 = buf;
  stop = 0;
  // Reading feature description
  while (!stop) {
    // If EOL reached --> continue at next line
    if (!*p1) {
      if (!fchars->next_line(cline)) {
        if (!fchars->eof())
          Rcpp::Rcerr << "Error reading " << fchars->get_name() << endl;
        return 0;
      }
      p1 = cline;
      *p2++ = ' ';  // insert blank
    }
    // Character description ends with '/' followed by blank or EOL
    if ((*p1=='/') && ((!*(p1+1)) || (*(p1+1)==' '))) {   // end of description
      *p2 = '\x00';
      stop = 1;
    }
    else {  // Continue
      *p2 = *p1;
      p1++; p2++;
    }
  }
  //----- Store character feature -----
  cd.feature = buf;
  cd.char_type = CT_TE;  // Default = text character. Will be overridden if
                         // numeric or multistate character (cf read_state())
  //----- Moves pointer at end of line or at the begin of next sentence -----
  p1++;
  while ((*p1==' ')||(*p1=='\t'))
    p1++;
  return 1;
}

//----- Reads a character state -----------------------------------------------
int tDeltaCharList::read_state(char *cline, char * & p1, int & nbstates)
{
  char buf[BUFSIZE];
  char *p2;
  int n, stop, unit;

  //----- Extract and verify the state number ------
  n = unit = 0;
  if (*p1>='0' && *p1<='9') {  // if there is a state number
    n = atoi(p1);
    if (n != nbstates+1) {
      Rcpp::Rcerr << "Parse error line " << fchars->get_lines_nb() << " : state sequence break" << endl;
      return 0;
    }
    nbstates++;
  }
  else
    unit = 1;   // "state" without number is a unit (numeric characters)
  //----- Extract state description -----
  // Skip at begin of state description
  while ((*p1>='0' && *p1<='9') || (*p1=='.') || (*p1==' '))
    p1++;
  p2 = buf;
  stop = 0;
  // Reading state description
  while (!stop) {
    // If EOL reached --> continue at next line
    if (!*p1) {
      if (!fchars->next_line(cline)) {
        if (!fchars->eof())
          Rcpp::Rcerr << "Error reading " << fchars->get_name() << endl;
        return 0;
      }
      p1 = cline;
      *p2++ = ' ';  // insert blank
    }
    // state description ends with '/' followed by blank or EOL
    if ((*p1=='/') && ((!*(p1+1)) || (*(p1+1)==' '))) {   // end of description
      *p2 = '\x00';
      stop = 1;
    }
    else {  // Continue
      *p2 = *p1;
      p1++; p2++;
    }
  }
  //----- Store unit or state description -----
  //--- Unit (numeric character)
  if (unit) {
    cd.unit = buf;
    cd.char_type = CT_IN;   // numeric character
  }
  else {
  //--- State description (multistate character)
    cd.states.push_back(buf);
    cd.char_type = CT_UM;  // multistate character
  }
  //----- Moves pointer at end of line or at begin next sentence -----
  p1++;
  while ((*p1==' ')||(*p1=='\t'))
    p1++;
  return 1;
}


//===== tDeltaItemList ========================================================

// Constructors

tDeltaItemList::tDeltaItemList(void)
{
  fitems = NULL;
  nbitems = 0;
  parsed = 0;
}

tDeltaItemList::tDeltaItemList(const char *fname, int parse)
{
  fitems = new tDeltaFile(fname);
  nbitems = 0;
  parsed = 0;
  if (parse)
    parse_items();
}


// Public member functions

//----- Parse the item file ---------------------------------------------------
int tDeltaItemList::parse_items(void)
{
  char cline[LMAXLINE];
  char *p1;
  int ok;

  //--- Test if items file exist ---
  if (!fitems)
    return 0;
  //--- Reset data from previous parsing
  if (parsed) {
    item_list.erase(item_list.begin(), item_list.end());
    directives.erase(directives.begin(), directives.end());
  }
  //--- Open the file ---
  if (!fitems->open(AM_READ)) {
    Rcpp::Rcerr << "Unable to open " << fitems->get_name() << endl;
    return 0;
  }
  ok = 1;
  p1 = NULL;
  //--- Reading loop ---
  while (ok) {
    if ((p1==NULL) || (!*p1)) {
      //--- Extract next line from item file ---
      ok = fitems->next_line(cline);
      if (!ok) {
        //--- End of file or error ---
        if (!fitems->eof()) {
          Rcpp::Rcerr << "Error reading " << fitems->get_name() << endl;
          return 0;
        }
        if (nbitems)   // at least one item is already read
          item_list.push_back(id);  // Store the last item read
        //--- End parsing ---
	parsed = 1;
        return 1;
      }
      p1 = cline;
    }
    //--- Processing the line ---
    if (*p1 == '*')
      //--- Reading a directive ---
      ok = read_directive(cline, p1);
    else
      if (*p1 == '#') {
        //--- Reading an item ---
        // Before reading the next item, we store the current one
        if (nbitems) {  // at least one item is already read
          item_list.push_back(id);
          // reinit attribute list
          id.attributes.erase(id.attributes.begin(), id.attributes.end());  
        }
        ok = read_item(cline, p1);
      }
      else
        Rcpp::Rcerr << "Error parsing " << fitems->get_name() << endl;
  }
  return 0;
}

//----- Set or change the item list filename ----------------------------------
void tDeltaItemList::set_filename(const char *fname, int parse)
{
  if (fitems)
    delete fitems;
  fitems = new tDeltaFile(fname);
  nbitems = 0;
  parsed = 0;
  if (parse)
    parse_items();
}

//----- Retrieves the item list filename --------------------------------------
const char *tDeltaItemList::get_filename(void)
{
  if (fitems)
    return fitems->get_name();
  else
    return "";
}

//----- Retrieves the number of items -----------------------------------------
int tDeltaItemList::get_items_nb(void) const
{
  return item_list.size();
}

//----- Retrieves an item name ------------------------------------------------
string tDeltaItemList::get_item_name(int itemnum, int comment) const
{
  if ((itemnum < 1) || (itemnum > (int)item_list.size()))
    return "";
  else
    // Returns item name with comments
    if (comment)
      return item_list[itemnum-1].name;
    // Returns item name after removing comments
    else {
      return remove_comments(item_list[itemnum-1].name);
    }
}

//----- Retrieves the number of attributes ------------------------------------
int tDeltaItemList::get_attributes_nb(int itemnum) const
{
  if ((itemnum < 1) || (itemnum > (int)item_list.size()))
    return 0;
  else
    return item_list[itemnum-1].attributes.size();
}

//----- Retrieves an item attribute -------------------------------------------
string tDeltaItemList::get_attribute(int itemnum, int attrnum) const
{
  string str;
  char ch[8];

  if ((itemnum < 1) || (itemnum > (int)item_list.size())) {
    return "";
  }
  if ((attrnum < 1) || (attrnum > (int)item_list[itemnum-1].attributes.size())) {
    return "";
  }
  sprintf(ch, "%d", item_list[itemnum-1].attributes[attrnum-1].get_charnum());
  str  = ch + item_list[itemnum-1].attributes[attrnum-1].get_charcomment()
         + "," + item_list[itemnum-1].attributes[attrnum-1].get_alternatives();
  return str;
}


//----- Searches the first item matching with a given character value(s) ------
int tDeltaItemList::first_matching(int charnum, double *values, int nbval,
                                   int strict, int with_extrval)
{
  int i, n;

  last_matching = 0;
  n = item_list.size();
  for (i=0; i<n; i++)
    if (item_list[i].matches(charnum, values, nbval, strict, with_extrval))
      last_matching = i+1;
  return last_matching;
}


//----- Searches the next item matching with a given character value(s) -------
int tDeltaItemList::next_matching(int charnum, double *values, int nbval,
                                  int strict, int with_extrval)
{
  int i, n, res=0;

  i = last_matching ? last_matching : 0;  // search begins after last matching item
  n = item_list.size();
  for ( ; i<n; i++)
    if (item_list[i].matches(charnum, values, nbval, strict, with_extrval))
      res = i+1;
  if (res) {
    last_matching = res;
    return last_matching;
  }
  return 0;
}

//----- Test if a given item is matching with values --------------------------
int tDeltaItemList::matches(int itemnum, int charnum, double *values, int nbval,
                            int strict, int with_extrval)
{
  if ((itemnum < 1) || (itemnum > (int)item_list.size())) {
    return 0;
  }
  return item_list[itemnum-1].matches(charnum, values, nbval, strict, with_extrval);
}

//----- Search a character in attributes list and make value(s) comparison ----
int tDeltaItemList::tItemDescr::matches(int charnum, double *values, int nbval,
                                        int strict, int with_extrval)
{
  int i, n;

  n = attributes.size();
  for (i=0; i<n; i++)
    if (attributes[i].get_charnum() == charnum)
      return attributes[i].compare(values, nbval, strict, with_extrval);
  //--- Character not found
  // Note : If the character is not found, his value is considered as UNKNOWN
  if (strict)
    return 0;  //UNKNOWN value never matching if "strict" compare (default)
  else
    return 1;
}

//----- For debuging
void tDeltaItemList::retrieve_all(void)
{
  int i,j;

  Rcpp::Rcout << endl <<"ITEM FILE : " << get_filename() << endl;
  Rcpp::Rcout << "Directives" << endl;
  Rcpp::Rcout << "----------" << endl;
  for (i=0; i<(int)directives.size(); i++)
    Rcpp::Rcout << directives[i] << endl;
  Rcpp::Rcout << "Item list" << endl;
  Rcpp::Rcout << "---------" << endl;
  for (i=0; i<(int)item_list.size(); i++) {
    Rcpp::Rcout << "Item " << (i+1) << " : " << item_list[i].name << endl;
    for (j=0; j < (int)item_list[i].attributes.size(); j++) {
      Rcpp::Rcout << "  Character " << item_list[i].attributes[j].get_charnum() << endl;
      string str = item_list[i].attributes[j].get_charcomment();
      if (str.size())
        Rcpp::Rcout << "    Comment : " << str << endl;
      str = item_list[i].attributes[j].get_alternatives();
      if (str.size())
        Rcpp::Rcout << "    Alternatives : " << str << endl;
    }
  }
}


// Private member functions

//----- Reads one directive -----------------------------------------------
int tDeltaItemList::read_directive(char *cline, char * & p1)
{
  directives.push_back(cline);
  p1 += strlen(cline);  // moves p1 at end of line
  return 1;
}

//----- Reads an item ----------------------------------------------------
int tDeltaItemList::read_item(char *cline, char * & p1)
{
  char nbuf[BUFSIZE];  // Item name buffer
  char *p2;
  int stop=0;

  nbitems++;
  //----- Extract item name and comment -----
  //--- Skip at begin of item name
  p1++;                              //skip '#'
  while ((*p1==' ') || (*p1=='\t'))  //skip blank(s) or tab(s)
    p1++;
  p2 = nbuf;
  //--- Reading loop
  while (!stop) {
    // If EOL reached --> continue at next line
    if (!*p1) {
      if (!fitems->next_line(cline)) {
        if (!fitems->eof())  // read error
          Rcpp::Rcerr << "Error reading " << fitems->get_name() << endl;
        else                 // EOF
          Rcpp::Rcerr << "Error parsing " << fitems->get_name() << " : item name without attributes" << endl;
        return 0;
      }
      p1 = cline;
      *p2++ = ' ';  // insert blank after line changing
    }
    // Item name and comment ends with '/' followed by blank or EOL
    if ((*p1=='/') && ((!*(p1+1)) || (*(p1+1)==' '))) {   // end of name
      *p2 = '\x00';
      id.name = nbuf;
      stop = 1;
    }
    else  // Continue
      *p2++ = *p1++;
  }  // end while (!stop)
  //----- Moves pointer at end of line or at the begin of next sentence -----
  p1++;
  while ((*p1==' ')||(*p1=='\t'))
    p1++;
  //----- Reads the item's attributes -----
  return read_attributes(cline, p1);
}

//----- Reads item attributes -----------------------------------------------
int tDeltaItemList::read_attributes(char *cline, char * & p1)
{
  char buf[BUFSIZE];
  char *p2;
  int stop;

  p2 = buf;
  stop = 0;
  //--- Reading item attributes
  while (!stop) {
    // If EOL reached --> continue at next line
    if (!*p1) {
      if (!fitems->next_line(cline)) {
        if (!fitems->eof()) {
          Rcpp::Rcerr << "Error reading " << fitems->get_name() << endl;
          return 0;
        }
        else {   // end of file
          *p2 = '\x00';
          if (*buf)
            return extract_attributes(buf);  // Extract attributes
          return 0;
        }
      }
      p1 = cline;
      // Items attributes ends at next item name (line beginning with #)
      if (*p1=='#') {
        *p2 = '\x00';
        stop = 1;
      }
      else
        *p2++ = ' ';  // insert blank after line changing
    }   // if (!*p1)
    else  // Continue
      *p2++ = *p1++;
  }
  //--- Extract attributes
  return extract_attributes(buf);
}

//----- Extract attributes from attribute list --------------------------------
int tDeltaItemList::extract_attributes(char *attrlst)
{
  tAttrDescr ad;
  char buf[BUFSIZE];
  char *p1, *p2;
  int comment;

  p1 = attrlst;
  p2 = buf;
  comment = 0;
  while (*p1) {
    //--- Blank (separator) --> end of the currrent attribute
    if ((*p1 == ' ') && (!comment)) {	// blanks in comment are ignored
      *p2 = '\x00';
      if (*buf) {   // ignore blank lines
        //--- Store attribute
        ad.parse_attr(buf);             // parse attribute
        id.attributes.push_back(ad);    // storing attribute
        p2 = buf;
      }
    }
    //--- Other characters (comments included) are copied into the buffer
    else {
      if (*p1 == '<')   // comment begin (nested comments are allowed)
        comment++;
      if ((*p1 == '>') && comment)   // comment end
        comment--;
      *p2 = *p1;
      p2++;
    }
    p1++;   // next char
  }
  //--- Store last attribute
  *p2 = '\x00';
  if (*buf) {   // ignore blank lines
    ad.parse_attr(buf);             // parse attribute
    id.attributes.push_back(ad);    // storing attribute
  }
  return 1;
}


//===== tAttrDescr ============================================================

//----- Extract comments (delimited by < >) from a string ---------------------
int tAttrDescr::extract_comment(char * & src, char *dest, int lmax)
{
  char *p1;
  int i;
  int comment=0;

  //--- Initialisation
  src++;  // skip '<'
  p1 = dest;
  i = 1;
  //--- Reading loop
  while (*src) {
    if (*src == '<')   // begin of a nested comment
      comment++;
    if (*src == '>') {
      if (comment)     // end of a nested comment
        comment--;
      else
        break;         // end main comment --> exit
    }
    if (i < lmax)
      *p1 = *src;
    src++; p1++;
    i++;
  }
  *p1= 0;
  //--- Ending
  if (!*src) {
    Rcpp::Rcerr << "Parse error : end of comment character '>' missing" << endl;
    return 0;  // abnormal
  }
  else
    src++;  // skip '>'
  return 1;    // normal
}

//----- Parses an attribute ---------------------------------------------------
int tAttrDescr::parse_attr(char *attr)
{
  tAltDescr altd;
  char buf[BUFSIZE];
  char *p1, *p2;

  //--- Clear previous data
  alternatives.erase(alternatives.begin(), alternatives.end());
  comment = "";
  alt = "";
  p1 = attr;
  //--- Extract character number
  charnum = atoi(p1);
  while ((*p1) && (*p1 != '<') && (*p1 != ','))  // Skip character number
    p1++;
  //--- Extract character comment
  if (*p1 == '<') {
    if (!extract_comment(p1, buf, BUFSIZE))
      return 0;
    comment = buf;
  }
  if (!*p1)
    return 0;  // Character without alternatives
  p1++;  // skip ','
  //--- Extract alternatives
  alt = p1;
  while (*p1) {
    // Copying an alternative into buf
    p2 = buf;
    while ((*p1) && (*p1 != '<') && (*p1 != '/')) {
      *p2 = *p1;
      p2++; p1++;
    }
    *p2 = 0;
    // Parse it (extract value list)
    altd.parse_alternative(buf);
    // Extract alternative comment
    if (*p1 == '<') {
      if (!extract_comment(p1, buf, BUFSIZE))
        return 0;
      altd.set_comment(buf);
    }
    else
      altd.set_comment("");  // deletes previous comment
    // Store the alternative into alternative list
    alternatives.push_back(altd);
    // Go to next alternative
    if (*p1)
      p1++;      // skip '/'
  }
  return 1;
}

//----- Browses alternatives list and makes value(s) comparison ---------------
int tAttrDescr::compare(double *values, int nbval, int strict, int with_extrval)
{
  int i, res;

  res = 0;
  for (i=0; (i < (int)alternatives.size()) && (!res); i++)
    res = alternatives[i].compare(values, nbval, strict, with_extrval);
  return res;
}


//===== tAltDescr =============================================================

//----- Parses an attribute ---------------------------------------------------
int tAltDescr::parse_alternative(char *altstr)
{
  char *p;
  double x;

  //--- Initialisation
  // Clear previous data
  value_list.values.erase(value_list.values.begin(), value_list.values.end());
  value_list.val_rel = value_list.extr_val = 0;
  p = altstr;
  //--- Reading special values
  switch (*p) {
    case 'V' :
      value_list.values.push_back(VARIABLE);
      return 1;
    case 'U' :
      value_list.values.push_back(UNKNOWN);
      return 1;
    case '-' :
      value_list.values.push_back(NOTAPPLI);
      return 1;
  }
  //--- Reading first value
  // Case of extreme low value
  if (*p == '(') {
    p++;    // skip (
    x = atof(p);
    value_list.values.push_back(x);
    value_list.extr_val = EXTRVAL_LOW;
    value_list.val_rel  = '-';
    while ((*p) && (*p != ')'))
      p++;
    if (*p == ')')
      p++;  // skip )
    if (!*p) {
      return 0;
    }
  }
  // First "normal" value
  x = atof(p);
  value_list.values.push_back(x);
  while ((*p) && ((*p != '-') && (*p != '&') && (*p != '(')))
    p++;
  if (!*p)
    return 1;
  if (*p != '(') {
    value_list.val_rel = *p;
    p++;
  }
  //--- Reading next values
  while (*p) {
    if (*p == '(') {    // case of extreme high value
      p++; p++;   // skip ( and -
      x = atof(p);
      value_list.values.push_back(x);
      value_list.extr_val |= EXTRVAL_HIGH;
      value_list.val_rel  = '-';
      break;
    }
    x = atof(p);
    value_list.values.push_back(x);
    while ((*p) && ((*p != value_list.val_rel) && (*p != '(')))
      p++;
    if (*p == value_list.val_rel)
      p++;
  }
  return 1;
}

//----- Comparison between alternative values and given value(s) --------------
int tAltDescr::compare(double *values, int nbval, int strict, int with_extrval)
{
  int first, last, i, j, res;

  //----- Unique value to compare
  if (!value_list.val_rel) {
    if (value_list.values[0] == VARIABLE)
      return 1;
    if (value_list.values[0] == UNKNOWN) {
      if (strict)
        return 0;
      else
        return 1;
    }
    if (value_list.values[0] == NOTAPPLI)
      return 0;
    return (*values == value_list.values[0]);
  }
  //----- Several values to compare with relation AND
  if (value_list.val_rel == '&') {
    first = 0;
    last = value_list.values.size()-1;
    for (i=first; i<=last; i++) {
      res = 0;
      for (j=0; j<nbval; j++)
        res |= (value_list.values[i] == values[j]);
      if (!res)
        break;
    }
    return res;
  }
  //----- Several values to compare with relation TO
  if (value_list.val_rel == '-') {
    first = 0;
    last = value_list.values.size()-1;
    if (!with_extrval) {
      if (value_list.extr_val && EXTRVAL_LOW)
        first++;
      if (value_list.extr_val && EXTRVAL_HIGH)
        last--;
    }
    res = 1;
    for (i=0; i<nbval; i++)
      res &= ((values[i] >= value_list.values[first]) &&
              (values[i] <= value_list.values[last]));
    return res;
  }
  return 0;
}


//===== tDeltaSpecs ===========================================================

//----- Delta specifications --------------------------------------------------

// Constructors

tDeltaSpecs::tDeltaSpecs(void)
{
  fspecs = NULL;
  parsed = 0;
}

tDeltaSpecs::tDeltaSpecs(const char *fname, tDeltaCharList *_chars, tDeltaItemList *_items, int parse)
{
  fspecs = new tDeltaFile(fname);
  chars = _chars;
  items = _items;
  impl_val.resize(chars->get_chars_nb());
  //--- Parsing
  parsed = 0;
  if (parse)
    parse_specs();
}


// Destructor

tDeltaSpecs::~tDeltaSpecs(void)
{
  delete fspecs;
}


// Public member functions

//----- Parse the specifications file -----------------------------------------
int tDeltaSpecs::parse_specs(void)
{
  char cline[LMAXLINE];
  char buf[BUFSIZE];
  char *p1;
  int n, ok;

  //--- Test if specifications file exist ---
  if (!fspecs)
    return 0;
  //--- Reset data from previous parsing ---
  // Specifications list
  n = specs_list.size();
  if (n)
    specs_list.erase(specs_list.begin(), specs_list.end());
  // Character dependencies
  n = char_dep.size();
  if (n) {
    char_dep.erase(char_dep.begin(), char_dep.end());
  }
  //--- Init implicit values table ---
  n = chars->get_chars_nb();
  impl_val.assign(n, tImplVal{0, 0});
  //--- Open the file ---
  if (!fspecs->open(AM_READ)) {
    Rcpp::Rcerr << "Unable to open " << fspecs->get_name() << endl;
    return 0;
  }
  *buf = '\x0';
  //--- Reading loop ---
  while (1) {
      ok = fspecs->next_line(cline);
      if (!ok) {
        //--- End of file or error ---
        if (!fspecs->eof()) {
          Rcpp::Rcerr << "Error reading " << fspecs->get_name() << endl;
          return 0;
        }
        if (*buf)   // at least one specification is already read
          specs_list.push_back(buf);  // Store the last specification read
        //--- End parsing ---
        parsed = 1;
        return parse_specs_detail();
      }
    //--- Processing the line ---
    if (*cline == '*') {  // new specification
      if (*buf)   // at least one specification is already read
        specs_list.push_back(buf);  // Store the last specification read
      strcpy(buf, cline+1);
    }
    else {           // additional specification line
      p1 = cline;
      while ((*p1==' ')||(*p1=='\t'))  // skip blank and tab at begin of line
        p1++;
      if ((*p1) && (*buf)) {
        strcat(buf, " ");    // separator
        strcat(buf, p1);
      }
    }
  }  // while (1)
  return 0;
}

//----- Set or change the specifications filename -----------------------------
void tDeltaSpecs::set_filename(const char *fname, int parse)
{
  if (fspecs)
    delete fspecs;
  fspecs = new tDeltaFile(fname);
  parsed = 0;
  if (parse)
    parse_specs();
}

//----- Retrieves the specifications filename ---------------------------------
const char *tDeltaSpecs::get_filename(void)
{
  if (fspecs)
    return fspecs->get_name();
  else
    return "";
}

//----- Retrieves implicit value of a character -------------------------------
int tDeltaSpecs::get_implicit_value(int charnum, int iv_type) const
{
  if (!is_parsed())
    return 0;
  if ((charnum < 1) || (charnum > chars->get_chars_nb())) {
    return 0;
  }
  switch (iv_type) {
    case 1 :
      return impl_val[charnum-1].iv1;
    case 2 :
      return impl_val[charnum-1].iv2;
    default :
      return 0;
  }
}

//----- Retrieves the number of dependent characters
//        for a given control character and state -----------------------------
int tDeltaSpecs::get_depchar_nb(int ccnum, int ccstate) const
{
  int st, i;

  // FIXED: Silent returns instead of error messages
  if ((ccnum < 1) || (ccnum > chars->get_chars_nb())) {
    return 0;
  }
  if ((ccstate < 1) || (ccstate > (int)sizeof(int))) {
    return 0;
  }
  st = 1 << (ccstate-1);
  for (i=0; i<(int)char_dep.size(); i++)
    if ((char_dep[i].cc == ccnum) && (char_dep[i].st & st))
      return char_dep[i].dcnb;
  return 0;
}

//----- Retrieves a dependent character (in 'rank' position)
//        for a given control character and state -----------------------------
int tDeltaSpecs::get_depchar(int ccnum, int ccstate, int rank) const
{
  int st, i, j, n, m;

  // FIXED: Silent returns instead of error messages
  if ((ccnum < 1) || (ccnum > chars->get_chars_nb())) {
    return 0;
  }
  if ((ccstate < 1) || (ccstate > (int)sizeof(int))) {
    return 0;
  }
  st = 1 << (ccstate-1);
  m = chars->get_chars_nb();
  for (i=0; i<(int)char_dep.size(); i++)
    if ((char_dep[i].cc == ccnum) && (char_dep[i].st & st)) {
      if ((rank >= 1) && (rank <= char_dep[i].dcnb)) {
        n = 0;
        for (j=0; j<m; j++) {
          if (char_dep[i].dc[j] == '1') {
            n++;
            if (n == rank)
              return (j+1);
          }
        }
      }
      else
        return 0;
    }
  return 0;
}


//----- Test if a character is dependent from
//        a given control character and state ---------------------------------
int tDeltaSpecs::is_dependent(int dcnum, int ccnum, int ccstate) const
{
  int st, i;

  // FIXED: Silent returns instead of error messages
  if ((ccnum < 1) || (ccnum > chars->get_chars_nb())) {
    return 0;
  }
  if ((dcnum < 1) || (dcnum > chars->get_chars_nb())) {
    return 0;
  }
  if ((ccstate < 1) || (ccstate > (int)sizeof(int))) {
    return 0;
  }
  st = 1 << (ccstate-1);
  for (i=0; i<(int)char_dep.size(); i++)
    if ((char_dep[i].cc == ccnum) && (char_dep[i].st & st))
      return (char_dep[i].dc[dcnum-1] == '1');
  return 0;
}


// For debugging
void tDeltaSpecs::retrieve_all(void)
{
  int i, j, n;

  Rcpp::Rcout << endl << "SPECIFICATION FILE : " << get_filename() << endl;
  Rcpp::Rcout << "Implicit values" << endl;
  Rcpp::Rcout << "---------------" << endl;
  n = chars->get_chars_nb();
  for (i=0; i<n; i++)
    if (impl_val[i].iv1) {
      Rcpp::Rcout << "Character " << i << " : " << impl_val[i].iv1;
      if (impl_val[i].iv2)
        Rcpp::Rcout << "/" << impl_val[i].iv2;
      Rcpp::Rcout << endl;
    }
  Rcpp::Rcout << "Character dependencies" << endl;
  Rcpp::Rcout << "----------------------" << endl;
  for (i=0; i<(int)char_dep.size(); i++) {
    Rcpp::Rcout << "Control character " << char_dep[i].cc << ", state(s) ";
    for (j=0; j<(int)sizeof(int); j++)
      if (char_dep[i].st & (1 << j))
        Rcpp::Rcout << (j+1) << " ";
    Rcpp::Rcout << endl;
    Rcpp::Rcout << "  Dependent character(s) : ";
    for (j=0; j<n; j++)
      if (char_dep[i].dc[j] == '1')
          Rcpp::Rcout << (j+1) << " ";
    Rcpp::Rcout << endl;
  }
}


// Protected member functions

//----- Parsing specific statements -------------------------------------------
int tDeltaSpecs::parse_specs_detail(void)
{
  const char *ch;

  for (int i=0; i<(int)specs_list.size(); i++) {
    if (!specs_list[i].find("CHARACTER TYPES")) {
      ch = specs_list[i].c_str();
      parse_char_types(ch);
    }
    if (!specs_list[i].find("IMPLICIT VALUES")) {
      ch = specs_list[i].c_str();
      parse_implicit_values(ch);
    }
    if (!specs_list[i].find("DEPENDENT CHARACTERS")) {
      ch = specs_list[i].c_str();
      parse_char_dependencies(ch);
    }
  }
  return 1;
}


//----- Parses "CHARACTER TYPES" statement ------------------------------------
void tDeltaSpecs::parse_char_types(const char *ch)
{
  const char *p1;
  int n1, n2, ct, ct0, i;

  p1 = ch + 15;
  while (*p1) {
    while (*p1==' ' || *p1=='\t')  // skip blanks and tabs
      p1++;
    if (!*p1)  // end of line
      return;
    //--- Reading character number
    n1 = atoi(p1);
    while (*p1>='0' && *p1<='9')
      p1++;
    if (*p1=='-') { // Range of character number
      p1++;
      n2 = atoi(p1);
      while (*p1>='0' && *p1<='9')
        p1++;
    }
    else
      n2 = n1;
    //--- Reading character type
    if (*p1==',') {
        p1++;
        ct = 0;
        if (!strncmp(p1, "OM", 2))
          ct = CT_OM;
        if (!strncmp(p1, "UM", 2))
          ct = CT_UM;
        if (!strncmp(p1, "TE", 2))
          ct = CT_TE;
        if (!strncmp(p1, "IN", 2))
          ct = CT_IN;
        if (!strncmp(p1, "RN", 2))
          ct = CT_RN;
        if (!ct) {
          Rcpp::Rcerr << "Error parsing " << get_filename() << " : erroneous character type" << endl;
          return;
        }
        //--- Storing character type
        for (i = n1; i <= n2; i++) {
          // Verify if character type is compatible with the one found in chars file
          ct0 = chars->get_char_type(i);
          if (((ct0 & CT_UM) && !(ct & CT_UM)) ||
              ((ct0 & CT_IN) && !(ct & CT_IN)) ||
              ((ct0 & CT_TE) && !((ct & CT_TE) || (ct & CT_IN)))) {
            Rcpp::Rcerr << "Error parsing " << get_filename() << " : " << endl
                 << "  Incompatible character type between chars and specs file for character " << i << endl;
          }
          else
            chars->set_char_type(i, ct);
        }
        p1++; p1++;
    }
    else {
      Rcpp::Rcerr << "Error parsing " << get_filename() << endl;
      return;
    }  // if (*p1==',')
  }  // while (p1)
}


//----- Parses "IMPLICIT VALUES" statement ------------------------------------
void tDeltaSpecs::parse_implicit_values(const char *ch)
{
  const char *p1;
  int char_nb, n1, n2, iv1, iv2, i;

  //--- Init implicit values table
  char_nb = chars->get_chars_nb();
  impl_val.assign(char_nb, tImplVal{0, 0});
  //--- Parsing implicit values line
  p1 = ch + 15;  // skip header
  while (*p1) {
    while (*p1==' ' || *p1=='\t')  // skip blanks and tabs
      p1++;
    if (!*p1)  // end of line
      break;
    // Reading character number
    n1 = atoi(p1);
    while (*p1>='0' && *p1<='9')
      p1++;
    if (*p1=='-') { // Range of character number
      p1++;
      n2 = atoi(p1);
      while (*p1>='0' && *p1<='9')
        p1++;
    }
    else
      n2 = n1;
    // Reading implicit values
    if (*p1==',') {
        p1++;
        iv1 = atoi(p1);
        while (*p1>='0' && *p1<='9')
          p1++;
        if (*p1==':') {
          p1++;
          iv2 = atoi(p1);
          while (*p1>='0' && *p1<='9')
            p1++;
        }
        else
          iv2 = 0;
        // Storing implicit values
        for (i=n1; i<=n2; i++) {
          impl_val[i-1].iv1 = iv1;
          impl_val[i-1].iv2 = iv2;
        }
    }
    else {
      Rcpp::Rcerr << "Error parsing " << get_filename() << endl;
      return;
    }  // if (*p1==',')
  }  // while (*p1)
}


//----- Parses "DEPENDANT CHARACTERS" statement -------------------------------
void tDeltaSpecs::parse_char_dependencies(const char *ch)
{
  const char *p1;
  int char_nb, dc1, dc2, i, n, stop;
  tCharDep cd;
  std::string dc_tmp;

  //--- Initialisation ---
  char_nb = chars->get_chars_nb();
  dc_tmp.assign(char_nb, '0');
  //--- Parsing character dependencies line ---
  p1 = ch + 20;  // skip header
  while (*p1) {
    while (*p1==' ' || *p1=='\t')  // skip blanks and tabs
      p1++;
    if (!*p1)  // end of line
      break;
    //--- Reading control character number
    cd.cc = atoi(p1);
    while (*p1>='0' && *p1<='9')
      p1++;
    if ((*p1==',') && (cd.cc > 0))
      p1++;
    else {
      Rcpp::Rcerr << "Error parsing 1 " << get_filename() << p1 << endl;
      break;
    }
    //--- Reading control character states
    stop = 0;
    cd.st = 0;
    while ((*p1) && (!stop)) {
      n = atoi(p1);
      if (n > 0) {
        if (n > (int)(sizeof(cd.st) * 8))
          Rcpp::Rcerr << "Too high state number, not considered"  << endl;
        else
          cd.st |= 1 << (n-1);
      } else {
        Rcpp::Rcerr << "Error parsing 2 " << get_filename() << endl;
        return;
      }
      while (*p1>='0' && *p1<='9')
        p1++;
      if (*p1=='/')
        p1++;
      else
        stop = 1;
    }
    if (*p1==':')
      p1++;
    else {
      Rcpp::Rcerr << "Error parsing 3 " << get_filename() << endl;
      return;
    }
    //--- Reading dependent characters
    cd.dcnb = 0;
    stop = 0;
    dc_tmp.assign(char_nb, '0');
    while ((*p1) && (!stop)) {
      dc1 = atoi(p1);
      while (*p1>='0' && *p1<='9')
        p1++;
      if (*p1=='-') { // Range of character number
        p1++;
        dc2 = atoi(p1);
        while (*p1>='0' && *p1<='9')
          p1++;
      }
      else
        dc2 = dc1;
      if ((dc1 > 0) && (dc2 > 0) && (dc2 >= dc1)) {
        for (i = dc1; i <= dc2; i++) {
          dc_tmp[i-1] = '1';
          cd.dcnb++;
        }
      } else {
        Rcpp::Rcerr << "Error parsing 4 " << get_filename() << endl;
        return;
      }
      if (*p1==':')
        p1++;
      else
        stop = 1;
    }
    //--- Storing character dependencies
    cd.dc = dc_tmp;
    char_dep.push_back(cd);
  }  // while (*p1)
}


//===== Other functions ===========================================================

//----- Removing comments from a string -------------------------------------------
//         Comments are delimited with brackets <>; nested comments are allowed.
string remove_comments(const std::string& src)
{
  std::string result;
  int comment = 0;
  
  for (char c : src) {
    if (c == '<') {
      comment++;
    } else if (c == '>') {
      if (comment > 0) comment--;
    } else if (comment == 0) {
      result += c;
    }
  }
  return result;
}