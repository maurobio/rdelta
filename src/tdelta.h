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
// File    : tdelta.h
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

#include <string>
#include <vector>
#include "tfile.h"

// FIX: Avoid using namespace std in header to prevent namespace pollution
// using namespace std;  // REMOVED - use explicit std:: instead

#define BUFSIZE 4096

//----- Character types
#define CT_UM 2  // unordered multistate (default multistate)
#define CT_OM 3  // ordered multistate
#define CT_IN 4  // integer numeric (default numeric)
#define CT_RN 5  // real numeric
#define CT_TE 8  // text

//---- Extreme character values
#define EXTRVAL_LOW  1  // extreme low value
#define EXTRVAL_HIGH 2  // extreme high value

//---- Special character values
#define VARIABLE -999999
#define UNKNOWN  -999998
#define NOTAPPLI -999997


//----- DeltaFile class (items or chars files) -----------------------------------
//        Derived from the generic tTextFile class
class tDeltaFile : public tTextFile {
  public :
    tDeltaFile(const char * _name) : tTextFile(_name)  { lines_nb = 0; }
    // Reads the next line from the file, with skipping empty lines and
    // deleting blank and tab characters at the begin of the line
    int next_line(char *dest, const int lmax = LMAXLINE);
    int get_lines_nb(void)  { return lines_nb; }
  protected :
    int lines_nb;  // number of lines
};


//----- Delta character list class -----------------------------------------------
class tDeltaCharList {
  public :
    //--- Constructors
    tDeltaCharList(void);
    tDeltaCharList(const char *fname, int parse=1);
         // fname = name of Delta character list file
         // parse = immediate parsing indicator
    // Destructor
    ~tDeltaCharList(void)  { delete fchars; }
    // Reading and parsing the character list file
    //    return value : 1=ok 0=error
    int parse_characters(void);
    // Set or change the character list file
    void set_filename(const char *fname, int parse=1);
    //--- Member functions returning character list information
    const char * get_filename(void);
    int is_parsed(void)  { return parsed; }
    // FIX: Added const qualifiers to getter methods
    int get_chars_nb(void) const;
    int get_char_type(int charnum) const;
    void set_char_type(int charnum, int chartype);
    std::string get_char_feature(int charnum) const;
    std::string get_char_unit(int charnum) const;
    int get_states_nb(int charnum) const;
    std::string get_state(int charnum, int statenum) const;
    // For debuging
    void retrieve_all(void);
  protected :
    tDeltaFile *fchars;
    //----- Delta character description class
    class tCharDescr {
      public :
        tCharDescr(void)  { }
        std::string feature;
        std::string unit;
        std::vector<std::string> states;
        int char_type;
    };
    std::vector<tCharDescr> char_list;
    tCharDescr cd;
    std::vector<std::string> directives;
    int parsed;   // file parsing flag
    int nbchars;  // number of characters
    int read_directive(char *cline, char * & p1);
    int read_character(char *cline, char * & p1);
    int read_state(char *cline, char * & p1, int & nbstates);
};


//----- Delta item description ------------------------------------------------
// - Items list is stored in tDeltaItemList class
// - Each item is described in tItemDescr class, which declaration is included
//   in tDeltaItemList. An item contains an attributes list.
// - Attributes (char number + values alternatives) are described in tAttrDescr
//   class. Each attribute contains an alternatives list.
// - Alternatives are described in tAltDescr class. Each alternative contains a
//   values list.
// - Values list are described in tValList class, which declaration is included
//   in tAltDescr class.
// See Delta format documentation and tDelta diagram for more information.

//--- Alternative description class ---------------------------------
class tAltDescr {
  public :
    tAltDescr(void)  { }
    int parse_alternative(char *altstr);
    // FIX: Change parameter to const char* to avoid warning
    void set_comment(const char *str) { comment = str; }
    std::string get_comment(void)  { return comment; }
    // Comparison between alternative values and given value(s)
    int compare(double *values, int nbval=1, int strict=1, int with_extrval=1);
      // values : pointer of a value or values table
      // nbval  : number of elements in values
      // strict (boolean) : strict comparison (comparison with UNKNOWN gives false)
      // with_extrval (boolean) : comparison with values including extreme values
  protected :
    // Values list class
    class tValList {
      public :
        tValList(void)  { }
        std::vector<double> values;
        char val_rel;  //values relation (0=unique value; '&'=and; '-'=to)
        int extr_val;  //extreme values (0=not; see #define EXTRVAL_...)
    };
    tValList value_list;
    std::string comment;    // optional comment
};

//--- Item attribute description class ------------------------------
class tAttrDescr {
  public :
    tAttrDescr(void)  { }
    int parse_attr(char *attr);
    // Member functions returning attribute information
    // FIX: Added const qualifiers to getter methods
    int get_charnum(void) const { return charnum; }
    std::string get_charcomment(void) const { return comment; }
    std::string get_alternatives(void) const { return alt; }
    int get_alt_nb(void)  { return alternatives.size(); }
    // Browses alternatives list and makes value(s) comparison
    int compare(double *values, int nbval=1, int strict=1, int with_extrval=1);
  protected :
    int charnum;                     // character number
    std::string comment;                  // optional comment (or value for text characters)
    std::vector<tAltDescr> alternatives;  // alternatives list
    std::string alt;                      // alternative list (not parsed)
    int extract_comment(char * & src, char *dest, int lmax);
      // Extracts comments from src string
};

//--- Delta item list class -----------------------------------------
class tDeltaItemList {
  public :
    //--- Constructors
    tDeltaItemList(void);
    tDeltaItemList(const char *fname, int parse=1);
         // fname = name of Delta character list file
         // parse = immediate parsing indicator
    // Destructor
    ~tDeltaItemList(void)  { delete fitems; }
    // Reading and parsing the item list file
    //    return value : 1=ok 0=error
    int parse_items(void);
    // Set or change the item list file
    void set_filename(const char *fname, int parse=1);
    //--- Member functions returning item list information
    const char * get_filename(void);
    int is_parsed(void)  { return parsed; }
    // FIX: Added const qualifiers to getter methods
    int get_items_nb(void) const;
    std::string get_item_name(int itemnum, int comment=1) const;
    int get_attributes_nb(int itemnum) const;
    std::string get_attribute(int itemnum, int attrnum) const;
    //--- Functions for identification :
    //      Test if given value(s) are matching with item attributes
    //      (see tAltDescr::compare for information about parameters)
    // First item matching with values
    int first_matching(int charnum, double *values, int nbval=1, int strict=1,
                       int with_extrval=1);  // returns item number or 0 if not found
    // Next item matching with values
    int next_matching(int charnum, double *values, int nbval=1, int strict=1,
                      int with_extrval=1);   // returns item number or 0 if not found
    // Test if a given item is matching with values
    int matches(int itemnum, int charnum, double *values, int nbval=1,
                int strict=1, int with_extrval=1); // returns 1 if matching, 0 elsewhere
    //--- For debuging
    void retrieve_all(void);
  protected :
    tDeltaFile *fitems;
    //--- Delta item description class
    class tItemDescr {
      public :
        tItemDescr(void)  { }
        std::string name;
        std::string comment;
        std::vector <tAttrDescr> attributes;
        // Search a character in attributes list and make value(s) comparison
        int matches(int charnum, double *values, int nbval=1, int strict=1,
                    int with_extrval=1);
    };
    tItemDescr id;
    std::vector<tItemDescr> item_list;
    std::vector<std::string> directives;
    std::string str;
    int parsed;   // file parsing flag
    int nbitems;  // number of items
    int last_matching;  // last item number matching with character value(s)
                        // after first_matching() or next_matching() call.
    int read_directive(char *cline, char * & p1);
    int read_item(char *cline, char * & p1);
    int read_attributes(char *cline, char * & p1);
    int extract_attributes(char *attrlst);
};


//----- Delta specifications --------------------------------------------------
class tDeltaSpecs {
  public :
    //--- Constructors
    tDeltaSpecs(void);
    tDeltaSpecs(const char *fname, tDeltaCharList *_chars, tDeltaItemList *_items, int parse=1);
         // fname = name of Delta specifications file
         // _chars, _items = pointers to characters and items list
         // parse = immediate parsing indicator
    // Destructor
    ~tDeltaSpecs(void);
    // Reading and parsing the specifications file
    //    return value : 1=ok 0=error
    int parse_specs(void);
    // Set or change the specifications file
    void set_filename(const char *fname, int parse=1);
    //--- Member functions returning specifications information
    const char * get_filename(void);
    // FIX: Added const qualifier to is_parsed
    int is_parsed(void) const { return parsed; }
    // FIX: Added const qualifiers to getter methods
    int get_implicit_value(int charnum, int iv_type=1) const;  // iv_type 1/2 --> returns iv1 or iv2
    // Retrieving informations about character dependencies
    //    ccnum, ccstate = control character number and state
    int get_depchar_nb(int ccnum, int ccstate) const;  // returns number of dependent characters
    int get_depchar(int ccnum, int ccstate, int rank=1) const;
        // returns the 'rank' dependent character
        // (rank=1 -> first dependent character, rank=2 -> second, ...)
    int is_dependent(int dcnum, int ccnum, int ccstate) const;
        // test if the 'dcnum' character is
        // dependent from control character 'ccnum' with state 'ccstate'
        // return value : 1=true, 0=false
    // For debuging
    void retrieve_all(void);
  protected :
    struct tImplVal {
      int iv1;  // Implicit value if character not specified in an item
      int iv2;  // Implicit value if character appears without a value in an item (optional)
    };
    // FIX: Changed char* dc to std::string
    struct tCharDep {
      int cc;    // Control character number
      int st;    // Control character states
      int dcnb;  // Number of dependent characters
      std::string dc;  // Dependent characters as string (was char*)
    };
    int parse_specs_detail(void);
    void parse_char_types(const char *ch);
    void parse_implicit_values(const char *ch);
    void parse_char_dependencies(const char *ch);
    tDeltaFile *fspecs;
    std::vector<std::string> specs_list;
    int parsed;   // file parsing flag
    // FIX: Changed from raw pointer to vector
    std::vector<tImplVal> impl_val;   // Implicit value table (was tImplVal*)
    std::vector<tCharDep> char_dep;  // Character dependencies list
    // Pointers to characters and items list associeted with the specifications
    tDeltaCharList *chars;
    tDeltaItemList *items;
    
    // FIX: Disable copy constructor and assignment operator to prevent double deletion
    // (These are not strictly required if we use vectors, but good practice)
    tDeltaSpecs(const tDeltaSpecs&) = delete;
    tDeltaSpecs& operator=(const tDeltaSpecs&) = delete;
};


//----- Delta class : main class ----------------------------------------------
//        including characters and items descriptions and specifications
class tDelta {
  public :
    tDelta(void)  { chars = NULL;  items = NULL;  specs = NULL; }
    tDelta(const char *chars_fname, const char *items_fname, int parse=1);
    tDelta(const char *chars_fname, const char *items_fname,
           const char *specs_fname, int parse=1);
      // .._fname = name of Delta characters, items list and specs files
      // parse = immediate parsing indicator
    ~tDelta(void);
    tDeltaCharList *chars;
    tDeltaItemList *items;
    tDeltaSpecs *specs;
  protected :
};


//----- Miscellaneous ---------------------------------------------------------

// FIX: Changed function signature to use std::string
// Removing comments (delimited by < >) from a string
std::string remove_comments(const std::string& src);
// Note: The old version was:
// void remove_comments(const char *src, char *dest);
// The new version returns a string by value