//==============================================================================
// RDelta - Rcpp Wrapper for tDelta Library (COMPLETE - ALL METHODS)
//==============================================================================

#include <Rcpp.h>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#include "tdelta.h"

using namespace Rcpp;

//==============================================================================
// RDelta Class - Complete Interface to tDelta (ALL METHODS EXPOSED)
//==============================================================================

class RDelta {
private:
  tDelta* delta;
  bool _has_specs;
  std::string last_error;
  
public:
  //----- Constructors --------------------------------------------------------
  
  RDelta(std::string chars_file, std::string items_file) {
    delta = new tDelta(chars_file.c_str(), items_file.c_str(), 1);
    _has_specs = false;
    if (!delta->chars->is_parsed() || !delta->items->is_parsed()) {
      throw std::runtime_error("Failed to parse DELTA files");
    }
  }
  
  RDelta(std::string chars_file, std::string items_file, 
         std::string specs_file) {
    delta = new tDelta(chars_file.c_str(), items_file.c_str(), 
                       specs_file.c_str(), 1);
    _has_specs = true;
    if (!delta->chars->is_parsed() || !delta->items->is_parsed() || 
        (delta->specs && !delta->specs->is_parsed())) {
      throw std::runtime_error("Failed to parse DELTA files");
    }
  }
  
  ~RDelta() {
    delete delta;
  }
  
  //==========================================================================
  // tDeltaCharList Methods - ALL EXPOSED
  //==========================================================================
  
  //----- File operations ----------------------------------------------------
  
  std::string get_filename() const {
    return std::string(delta->chars->get_filename());
  }
  
  bool is_parsed() const {
    return delta->chars->is_parsed() == 1;
  }
  
  void set_filename(std::string fname) {
    delta->chars->set_filename(fname.c_str(), 1);
  }
  
  void set_filename_parse(std::string fname, bool parse) {
    delta->chars->set_filename(fname.c_str(), parse ? 1 : 0);
  }
  
  bool parse_characters() {
    return delta->chars->parse_characters() == 1;
  }
  
  //----- Character information ---------------------------------------------
  
  int get_chars_nb() const {
    return delta->chars->get_chars_nb();
  }
  
  int get_char_type(int charnum) const {
    if (charnum < 1 || charnum > get_chars_nb()) {
      throw std::out_of_range("Character number out of range");
    }
    return delta->chars->get_char_type(charnum);
  }
  
  void set_char_type(int charnum, int chartype) {
    if (charnum < 1 || charnum > get_chars_nb()) {
      throw std::out_of_range("Character number out of range");
    }
    delta->chars->set_char_type(charnum, chartype);
  }
  
  std::string get_char_feature(int charnum) const {
    if (charnum < 1 || charnum > get_chars_nb()) {
      throw std::out_of_range("Character number out of range");
    }
    return delta->chars->get_char_feature(charnum);
  }
  
  std::string get_char_unit(int charnum) const {
    if (charnum < 1 || charnum > get_chars_nb()) {
      throw std::out_of_range("Character number out of range");
    }
    return delta->chars->get_char_unit(charnum);
  }
  
  //----- State information --------------------------------------------------
  
  int get_states_nb(int charnum) const {
    if (charnum < 1 || charnum > get_chars_nb()) {
      throw std::out_of_range("Character number out of range");
    }
    return delta->chars->get_states_nb(charnum);
  }
  
  std::string get_state(int charnum, int statenum) const {
    if (charnum < 1 || charnum > get_chars_nb()) {
      throw std::out_of_range("Character number out of range");
    }
    if (statenum < 1 || statenum > get_states_nb(charnum)) {
      throw std::out_of_range("State number out of range");
    }
    return delta->chars->get_state(charnum, statenum);
  }
  
  //----- R-friendly module methods -----------------------------------------
  
  int char_count() const {
    return get_chars_nb();
  }
  
  std::string char_feature(int charnum) const {
    return get_char_feature(charnum);
  }
  
  std::string char_unit(int charnum) const {
    return get_char_unit(charnum);
  }
  
  int char_type(int charnum) const {
    return get_char_type(charnum);
  }
  
  std::string char_type_name(int charnum) const {
    int ct = get_char_type(charnum);
    switch (ct) {
      case CT_UM: return "unordered_multistate";
      case CT_OM: return "ordered_multistate";
      case CT_IN: return "integer_numeric";
      case CT_RN: return "real_numeric";
      case CT_TE: return "text";
      default: return "unknown";
    }
  }
  
  int state_count(int charnum) const {
    return get_states_nb(charnum);
  }
  
  std::string state_name(int charnum, int statenum) const {
    return get_state(charnum, statenum);
  }
  
  std::vector<std::string> get_states(int charnum) const {
    std::vector<std::string> result;
    int n = get_states_nb(charnum);
    for (int i = 1; i <= n; i++) {
      result.push_back(get_state(charnum, i));
    }
    return result;
  }
  
  std::vector<std::string> states(int charnum) const {
    return get_states(charnum);
  }
  
  std::string get_char_type_name(int charnum) const {
    int ct = get_char_type(charnum);
    switch (ct) {
      case CT_UM: return "unordered_multistate";
      case CT_OM: return "ordered_multistate";
      case CT_IN: return "integer_numeric";
      case CT_RN: return "real_numeric";
      case CT_TE: return "text";
      default: return "unknown";
    }
  }
  
  void set_char_type_by_name(int charnum, std::string type_name) {
    int ct = 0;
    if (type_name == "unordered_multistate") ct = CT_UM;
    else if (type_name == "ordered_multistate") ct = CT_OM;
    else if (type_name == "integer_numeric") ct = CT_IN;
    else if (type_name == "real_numeric") ct = CT_RN;
    else if (type_name == "text") ct = CT_TE;
    else throw std::invalid_argument("Invalid character type name");
    set_char_type(charnum, ct);
  }
  
  //----- Debug --------------------------------------------------------------
  
  std::string get_chars_debug() const {
    std::stringstream buffer;
    std::streambuf* old = Rcpp::Rcout.rdbuf(buffer.rdbuf());
    delta->chars->retrieve_all();
    Rcpp::Rcout.rdbuf(old);
    return buffer.str();
  }
  
  //==========================================================================
  // tDeltaItemList Methods - ALL EXPOSED
  //==========================================================================
  
  //----- File operations ----------------------------------------------------
  
  std::string get_items_filename() const {
    return std::string(delta->items->get_filename());
  }
  
  bool is_items_parsed() const {
    return delta->items->is_parsed() == 1;
  }
  
  void set_items_filename(std::string fname) {
    delta->items->set_filename(fname.c_str(), 1);
  }
  
  void set_items_filename_parse(std::string fname, bool parse) {
    delta->items->set_filename(fname.c_str(), parse ? 1 : 0);
  }
  
  bool parse_items() {
    return delta->items->parse_items() == 1;
  }
  
  //----- Item information ---------------------------------------------------
  
  int get_items_nb() const {
    return delta->items->get_items_nb();
  }
  
  std::string get_item_name(int itemnum) const {
    if (itemnum < 1 || itemnum > get_items_nb()) {
      throw std::out_of_range("Item number out of range");
    }
    return delta->items->get_item_name(itemnum, 1);
  }
  
  std::string get_item_name_comments(int itemnum, bool include_comments) const {
    if (itemnum < 1 || itemnum > get_items_nb()) {
      throw std::out_of_range("Item number out of range");
    }
    return delta->items->get_item_name(itemnum, include_comments ? 1 : 0);
  }
  
  int get_attributes_nb(int itemnum) const {
    if (itemnum < 1 || itemnum > get_items_nb()) {
      throw std::out_of_range("Item number out of range");
    }
    return delta->items->get_attributes_nb(itemnum);
  }
  
  std::string get_attribute(int itemnum, int attrnum) const {
    if (itemnum < 1 || itemnum > get_items_nb()) {
      throw std::out_of_range("Item number out of range");
    }
    if (attrnum < 1 || attrnum > get_attributes_nb(itemnum)) {
      throw std::out_of_range("Attribute number out of range");
    }
    return delta->items->get_attribute(itemnum, attrnum);
  }
  
  //----- R-friendly module methods ----------------------------------------
  
  int item_count() const {
    return get_items_nb();
  }
  
  std::string item_name(int itemnum) const {
    return get_item_name(itemnum);
  }
  
  std::string item_name_comments(int itemnum, bool include_comments) const {
    return get_item_name_comments(itemnum, include_comments);
  }
  
  std::vector<std::string> get_item_names() const {
    std::vector<std::string> result;
    int n = get_items_nb();
    for (int i = 1; i <= n; i++) {
      result.push_back(get_item_name(i));
    }
    return result;
  }
  
  std::vector<std::string> get_item_names_comments(bool include_comments) const {
    std::vector<std::string> result;
    int n = get_items_nb();
    for (int i = 1; i <= n; i++) {
      result.push_back(get_item_name_comments(i, include_comments));
    }
    return result;
  }
  
  std::vector<std::string> item_names() const {
    return get_item_names();
  }
  
  std::vector<std::string> item_names_comments(bool include_comments) const {
    return get_item_names_comments(include_comments);
  }
  
  int attribute_count(int itemnum) const {
    return get_attributes_nb(itemnum);
  }
  
  std::string attribute_string(int itemnum, int attrnum) const {
    return get_attribute(itemnum, attrnum);
  }
  
  DataFrame get_item_attributes(int itemnum) const {
    int n = get_attributes_nb(itemnum);
    IntegerVector chars(n);
    CharacterVector attr_strings(n);
    CharacterVector comments(n);
    
    for (int i = 0; i < n; i++) {
      std::string attr = get_attribute(itemnum, i + 1);
      
      size_t comma_pos = attr.find(",");
      if (comma_pos != std::string::npos) {
        try {
          chars[i] = std::stoi(attr.substr(0, comma_pos));
        } catch (...) {
          chars[i] = NA_INTEGER;
        }
        size_t comment_start = attr.find("<");
        size_t comment_end = attr.find(">");
        if (comment_start != std::string::npos && 
            comment_end != std::string::npos && 
            comment_start < comment_end) {
          comments[i] = attr.substr(comment_start + 1, comment_end - comment_start - 1);
        }
        attr_strings[i] = attr;
      } else {
        chars[i] = NA_INTEGER;
        attr_strings[i] = attr;
      }
    }
    
    return DataFrame::create(
      _["character"] = chars,
      _["attribute"] = attr_strings,
      _["comment"] = comments,
      _["stringsAsFactors"] = false
    );
  }
  
  DataFrame item_attributes(int itemnum) const {
    return get_item_attributes(itemnum);
  }
  
  List get_item_data(int itemnum) const {
    List result;
    result["item_num"] = itemnum;
    result["item_name"] = get_item_name(itemnum);
    result["attributes"] = get_item_attributes(itemnum);
    return result;
  }
  
  List get_item_data_comments(int itemnum, bool include_comments) const {
    List result;
    result["item_num"] = itemnum;
    result["item_name"] = get_item_name_comments(itemnum, include_comments);
    result["attributes"] = get_item_attributes(itemnum);
    return result;
  }
  
  List item_data(int itemnum) const {
    return get_item_data(itemnum);
  }
  
  List item_data_comments(int itemnum, bool include_comments) const {
    return get_item_data_comments(itemnum, include_comments);
  }
  
  //----- Search and matching ------------------------------------------------
  
  int first_matching(int charnum, NumericVector values,
                     bool strict, bool with_extrval) {
    double* vals = REAL(values);
    return delta->items->first_matching(charnum, vals, values.size(),
                                        strict ? 1 : 0, with_extrval ? 1 : 0);
  }
  
  int first_matching_default(int charnum, NumericVector values) {
    return first_matching(charnum, values, true, true);
  }
  
  int next_matching(int charnum, NumericVector values,
                    bool strict, bool with_extrval) {
    double* vals = REAL(values);
    return delta->items->next_matching(charnum, vals, values.size(),
                                       strict ? 1 : 0, with_extrval ? 1 : 0);
  }
  
  int next_matching_default(int charnum, NumericVector values) {
    return next_matching(charnum, values, true, true);
  }
  
  bool matches(int itemnum, int charnum, NumericVector values,
               bool strict, bool with_extrval) const {
    if (itemnum < 1 || itemnum > get_items_nb()) {
      throw std::out_of_range("Item number out of range");
    }
    double* vals = const_cast<double*>(REAL(values));
    return delta->items->matches(itemnum, charnum, vals, values.size(),
                                 strict ? 1 : 0, with_extrval ? 1 : 0) == 1;
  }
  
  bool matches_default(int itemnum, int charnum, NumericVector values) const {
    return matches(itemnum, charnum, values, true, true);
  }
  
  IntegerVector find_all_matching(int charnum, NumericVector values,
                                  bool strict, bool with_extrval) {
    IntegerVector results;
    double* vals = REAL(values);
    int nbval = values.size();
    
    int match = delta->items->first_matching(
      charnum, vals, nbval, strict ? 1 : 0, with_extrval ? 1 : 0
    );
    
    while (match > 0) {
      results.push_back(match);
      match = delta->items->next_matching(
        charnum, vals, nbval, strict ? 1 : 0, with_extrval ? 1 : 0
      );
    }
    
    return results;
  }
  
  IntegerVector find_all_matching_default(int charnum, NumericVector values) {
    return find_all_matching(charnum, values, true, true);
  }
  
  IntegerVector find_matching(int charnum, NumericVector values,
                              bool strict, bool with_extrval) {
    return find_all_matching(charnum, values, strict, with_extrval);
  }
  
  IntegerVector find_matching_default(int charnum, NumericVector values) {
    return find_all_matching_default(charnum, values);
  }
  
  //----- Debug --------------------------------------------------------------
  
  std::string get_items_debug() const {
    std::stringstream buffer;
    std::streambuf* old = Rcpp::Rcout.rdbuf(buffer.rdbuf());
    delta->items->retrieve_all();
    Rcpp::Rcout.rdbuf(old);
    return buffer.str();
  }
  
  //==========================================================================
  // tDeltaSpecs Methods - ALL EXPOSED (FIXED: NO ERROR MESSAGES)
  //==========================================================================
  
  bool has_specifications() const {
    return _has_specs && delta->specs != NULL;
  }
  
  //----- File operations ----------------------------------------------------
  
  std::string get_specs_filename() const {
    if (!_has_specs || delta->specs == NULL) return "";
    return std::string(delta->specs->get_filename());
  }
  
  bool is_specs_parsed() const {
    return _has_specs && delta->specs != NULL && delta->specs->is_parsed() == 1;
  }
  
  void set_specs_filename(std::string fname) {
    if (!_has_specs || delta->specs == NULL) {
      throw std::runtime_error("No specifications object available");
    }
    delta->specs->set_filename(fname.c_str(), 1);
  }
  
  void set_specs_filename_parse(std::string fname, bool parse) {
    if (!_has_specs || delta->specs == NULL) {
      throw std::runtime_error("No specifications object available");
    }
    delta->specs->set_filename(fname.c_str(), parse ? 1 : 0);
  }
  
  bool parse_specs() {
    if (!_has_specs || delta->specs == NULL) {
      throw std::runtime_error("No specifications object available");
    }
    return delta->specs->parse_specs() == 1;
  }
  
  //----- Implicit values ----------------------------------------------------
  
  // NEW: Get implicit value with type parameter
  int get_implicit_value(int charnum, int iv_type) const {
    if (!has_specifications()) {
      return 0;  // Silent return
    }
    if (charnum < 1 || charnum > get_chars_nb()) {
      return 0;  // Silent return
    }
    return delta->specs->get_implicit_value(charnum, iv_type);
  }
  
  // NEW: Get implicit value with default type (1)
  int get_implicit_value_default(int charnum) const {
    return get_implicit_value(charnum, 1);
  }
  
  // NEW: Get implicit value (R-friendly)
  int implicit_value(int charnum, int iv_type) const {
    return get_implicit_value(charnum, iv_type);
  }
  
  // NEW: Get implicit value default (R-friendly)
  int implicit_value_default(int charnum) const {
    return get_implicit_value_default(charnum);
  }
  
  // NEW: Get all implicit values for all characters
  IntegerVector get_all_implicit_values(int iv_type) const {
    IntegerVector result(get_chars_nb());
    if (!has_specifications()) {
      for (int i = 0; i < get_chars_nb(); i++) {
        result[i] = NA_INTEGER;
      }
      return result;
    }
    for (int i = 1; i <= get_chars_nb(); i++) {
      try {
        result[i-1] = delta->specs->get_implicit_value(i, iv_type);
      } catch (...) {
        result[i-1] = NA_INTEGER;
      }
    }
    return result;
  }
  
  IntegerVector get_all_implicit_values_default() const {
    return get_all_implicit_values(1);
  }
  
  IntegerVector implicit_values(int iv_type) const {
    return get_all_implicit_values(iv_type);
  }
  
  IntegerVector implicit_values_default() const {
    return get_all_implicit_values_default();
  }
  
  //----- Character dependencies - FIXED: No error messages ----------------
  
  // NEW: Get number of dependent characters for a control character and state
  int get_depchar_nb(int ccnum, int ccstate) const {
    if (!has_specifications()) return 0;
    if (ccnum < 1 || ccnum > get_chars_nb()) {
      return 0;  // Silent return
    }
    // Check if the state is valid for this character
    int n_states = get_states_nb(ccnum);
    if (ccstate < 1 || ccstate > n_states) {
      return 0;  // State out of range, silently return 0
    }
    return delta->specs->get_depchar_nb(ccnum, ccstate);
  }
  
  // NEW: R-friendly version
  int dependent_count(int ccnum, int ccstate) const {
    return get_depchar_nb(ccnum, ccstate);
  }
  
  // NEW: Get dependent character at a specific rank
  int get_depchar(int ccnum, int ccstate, int rank) const {
    if (!has_specifications()) return 0;
    if (ccnum < 1 || ccnum > get_chars_nb()) {
      return 0;
    }
    int n_states = get_states_nb(ccnum);
    if (ccstate < 1 || ccstate > n_states) {
      return 0;
    }
    return delta->specs->get_depchar(ccnum, ccstate, rank);
  }
  
  // NEW: Get first dependent character (default rank = 1)
  int get_depchar_default(int ccnum, int ccstate) const {
    return get_depchar(ccnum, ccstate, 1);
  }
  
  // NEW: Get all dependent characters for a control character and state
  IntegerVector get_all_depchar(int ccnum, int ccstate) const {
    IntegerVector result;
    if (!has_specifications()) return result;
    if (ccnum < 1 || ccnum > get_chars_nb()) {
      return result;
    }
    int n_states = get_states_nb(ccnum);
    if (ccstate < 1 || ccstate > n_states) {
      return result;
    }
    
    int n = delta->specs->get_depchar_nb(ccnum, ccstate);
    for (int i = 1; i <= n; i++) {
      int dep = delta->specs->get_depchar(ccnum, ccstate, i);
      if (dep > 0) result.push_back(dep);
    }
    return result;
  }
  
  // NEW: R-friendly version
  IntegerVector dependent_characters(int ccnum, int ccstate) const {
    return get_all_depchar(ccnum, ccstate);
  }
  
  // NEW: Check if a character is dependent on a control character and state
  bool is_dependent(int dcnum, int ccnum, int ccstate) const {
    if (!has_specifications()) return false;
    if (dcnum < 1 || dcnum > get_chars_nb() || ccnum < 1 || ccnum > get_chars_nb()) {
      return false;
    }
    int n_states = get_states_nb(ccnum);
    if (ccstate < 1 || ccstate > n_states) {
      return false;
    }
    return delta->specs->is_dependent(dcnum, ccnum, ccstate) == 1;
  }
  
  // NEW: Get all dependency relationships as a list
  List get_all_dependencies() const {
    List result;
    if (!has_specifications()) return result;
    
    // Get character count
    int n_chars = get_chars_nb();
    
    // For each character, check each state
    for (int ccnum = 1; ccnum <= n_chars; ccnum++) {
      int n_states = get_states_nb(ccnum);
      for (int ccstate = 1; ccstate <= n_states; ccstate++) {
        int dep_count = get_depchar_nb(ccnum, ccstate);
        if (dep_count > 0) {
          IntegerVector deps = get_all_depchar(ccnum, ccstate);
          if (deps.size() > 0) {
            CharacterVector key(1);
            key[0] = std::to_string(ccnum) + "," + std::to_string(ccstate);
            result[key] = deps;
          }
        }
      }
    }
    return result;
  }
  
  //----- R-friendly module methods -----------------------------------------
  
  std::string specs_filename() const {
    return get_specs_filename();
  }
  
  bool specs_parsed() const {
    return is_specs_parsed();
  }
  
  //----- Debug --------------------------------------------------------------
  
  std::string get_specs_debug() const {
    if (!_has_specs || delta->specs == NULL) {
      return "No specifications available";
    }
    std::stringstream buffer;
    std::streambuf* old = Rcpp::Rcout.rdbuf(buffer.rdbuf());
    delta->specs->retrieve_all();
    Rcpp::Rcout.rdbuf(old);
    return buffer.str();
  }
  
  //==========================================================================
  // Debug Methods - retrieve_all (matching C++ original)
  //==========================================================================
  
  std::string retrieve_all_chars() const {
    std::stringstream buffer;
    std::streambuf* old = Rcpp::Rcout.rdbuf(buffer.rdbuf());
    delta->chars->retrieve_all();
    Rcpp::Rcout.rdbuf(old);
    return buffer.str();
  }
  
  std::string retrieve_all_items() const {
    std::stringstream buffer;
    std::streambuf* old = Rcpp::Rcout.rdbuf(buffer.rdbuf());
    delta->items->retrieve_all();
    Rcpp::Rcout.rdbuf(old);
    return buffer.str();
  }
  
  std::string retrieve_all_specs() const {
    if (!_has_specs || delta->specs == NULL) {
      return "No specifications available";
    }
    std::stringstream buffer;
    std::streambuf* old = Rcpp::Rcout.rdbuf(buffer.rdbuf());
    delta->specs->retrieve_all();
    Rcpp::Rcout.rdbuf(old);
    return buffer.str();
  }
  
  std::string retrieve_all() const {
    std::stringstream result;
    
    result << "========================================\n";
    result << "     CHARACTER FILE DEBUG\n";
    result << "========================================\n";
    result << retrieve_all_chars();
    
    result << "\n========================================\n";
    result << "     ITEM FILE DEBUG\n";
    result << "========================================\n";
    result << retrieve_all_items();
    
    if (_has_specs && delta->specs != NULL) {
      result << "\n========================================\n";
      result << "     SPECIFICATIONS FILE DEBUG\n";
      result << "========================================\n";
      result << retrieve_all_specs();
    }
    
    return result.str();
  }
  
  //==========================================================================
  // Utility Methods
  //==========================================================================
  
  std::string get_version() const {
    return "0.20 (August 2001) - R integration";
  }
  
  std::string get_all_debug() const {
    std::stringstream result;
    result << "=== Characters ===\n";
    result << get_chars_debug();
    result << "\n=== Items ===\n";
    result << get_items_debug();
    if (has_specifications()) {
      result << "\n=== Specifications ===\n";
      result << get_specs_debug();
    }
    return result.str();
  }
};

//==============================================================================
// RCPP MODULE DEFINITION - COMPLETE
//==============================================================================

RCPP_MODULE(delta_module) {
  
  class_<RDelta>("DeltaParser")
    
    //----- Constructors ----------------------------------------------------
    .constructor<std::string, std::string>()
    .constructor<std::string, std::string, std::string>()
    
    //----- Character methods - C++ style ----------------------------------
    .method("get_filename", &RDelta::get_filename)
    .method("is_parsed", &RDelta::is_parsed)
    .method("set_filename", &RDelta::set_filename)
    .method("set_filename_parse", &RDelta::set_filename_parse)
    .method("parse_characters", &RDelta::parse_characters)
    .method("get_chars_nb", &RDelta::get_chars_nb)
    .method("get_char_type", &RDelta::get_char_type)
    .method("set_char_type", &RDelta::set_char_type)
    .method("get_char_feature", &RDelta::get_char_feature)
    .method("get_char_unit", &RDelta::get_char_unit)
    .method("get_states_nb", &RDelta::get_states_nb)
    .method("get_state", &RDelta::get_state)
    
    //----- Character methods - R-friendly ---------------------------------
    .method("char_count", &RDelta::char_count)
    .method("char_feature", &RDelta::char_feature)
    .method("char_unit", &RDelta::char_unit)
    .method("char_type", &RDelta::char_type)
    .method("char_type_name", &RDelta::char_type_name)
    .method("state_count", &RDelta::state_count)
    .method("state_name", &RDelta::state_name)
    .method("states", &RDelta::states)
    .method("get_states", &RDelta::get_states)
    .method("set_char_type_by_name", &RDelta::set_char_type_by_name)
    .method("get_char_type_name", &RDelta::get_char_type_name)
    .method("get_chars_debug", &RDelta::get_chars_debug)
    
    //----- Item methods - C++ style ---------------------------------------
    .method("get_items_filename", &RDelta::get_items_filename)
    .method("is_items_parsed", &RDelta::is_items_parsed)
    .method("set_items_filename", &RDelta::set_items_filename)
    .method("set_items_filename_parse", &RDelta::set_items_filename_parse)
    .method("parse_items", &RDelta::parse_items)
    .method("get_items_nb", &RDelta::get_items_nb)
    .method("get_item_name", &RDelta::get_item_name)
    .method("get_item_name_comments", &RDelta::get_item_name_comments)
    .method("get_attributes_nb", &RDelta::get_attributes_nb)
    .method("get_attribute", &RDelta::get_attribute)
    
    //----- Item methods - R-friendly --------------------------------------
    .method("item_count", &RDelta::item_count)
    .method("item_name", &RDelta::item_name)
    .method("item_name_comments", &RDelta::item_name_comments)
    .method("item_names", &RDelta::item_names)
    .method("item_names_comments", &RDelta::item_names_comments)
    .method("get_item_names", &RDelta::get_item_names)
    .method("get_item_names_comments", &RDelta::get_item_names_comments)
    .method("attribute_count", &RDelta::attribute_count)
    .method("attribute_string", &RDelta::attribute_string)
    .method("item_attributes", &RDelta::item_attributes)
    .method("get_item_attributes", &RDelta::get_item_attributes)
    .method("item_data", &RDelta::item_data)
    .method("item_data_comments", &RDelta::item_data_comments)
    .method("get_item_data", &RDelta::get_item_data)
    .method("get_item_data_comments", &RDelta::get_item_data_comments)
    .method("get_items_debug", &RDelta::get_items_debug)
    
    //----- Search methods ------------------------------------------------
    .method("first_matching", &RDelta::first_matching)
    .method("first_matching_default", &RDelta::first_matching_default)
    .method("next_matching", &RDelta::next_matching)
    .method("next_matching_default", &RDelta::next_matching_default)
    .method("matches", &RDelta::matches)
    .method("matches_default", &RDelta::matches_default)
    .method("find_matching", &RDelta::find_matching)
    .method("find_matching_default", &RDelta::find_matching_default)
    .method("find_all_matching", &RDelta::find_all_matching)
    .method("find_all_matching_default", &RDelta::find_all_matching_default)
    
    //----- Specifications methods - C++ style ----------------------------
    .method("has_specifications", &RDelta::has_specifications)
    .method("get_specs_filename", &RDelta::get_specs_filename)
    .method("is_specs_parsed", &RDelta::is_specs_parsed)
    .method("set_specs_filename", &RDelta::set_specs_filename)
    .method("set_specs_filename_parse", &RDelta::set_specs_filename_parse)
    .method("parse_specs", &RDelta::parse_specs)
    
    //----- Implicit values methods - ALL NEW -----------------------------
    .method("get_implicit_value", &RDelta::get_implicit_value)
    .method("get_implicit_value_default", &RDelta::get_implicit_value_default)
    .method("get_all_implicit_values", &RDelta::get_all_implicit_values)
    .method("get_all_implicit_values_default", &RDelta::get_all_implicit_values_default)
    .method("implicit_value", &RDelta::implicit_value)
    .method("implicit_value_default", &RDelta::implicit_value_default)
    .method("implicit_values", &RDelta::implicit_values)
    .method("implicit_values_default", &RDelta::implicit_values_default)
    
    //----- Dependent characters methods - ALL NEW ------------------------
    .method("get_depchar_nb", &RDelta::get_depchar_nb)
    .method("get_depchar", &RDelta::get_depchar)
    .method("get_depchar_default", &RDelta::get_depchar_default)
    .method("get_all_depchar", &RDelta::get_all_depchar)
    .method("is_dependent", &RDelta::is_dependent)
    .method("dependent_count", &RDelta::dependent_count)
    .method("dependent_characters", &RDelta::dependent_characters)
    .method("get_all_dependencies", &RDelta::get_all_dependencies)
    
    //----- Specifications methods - R-friendly ---------------------------
    .method("specs_filename", &RDelta::specs_filename)
    .method("specs_parsed", &RDelta::specs_parsed)
    .method("get_specs_debug", &RDelta::get_specs_debug)
    
    //----- Debug methods (C++ style retrieve_all) ------------------------
    .method("retrieve_all_chars", &RDelta::retrieve_all_chars)
    .method("retrieve_all_items", &RDelta::retrieve_all_items)
    .method("retrieve_all_specs", &RDelta::retrieve_all_specs)
    .method("retrieve_all", &RDelta::retrieve_all)
    
    //----- Utility --------------------------------------------------------
    .method("get_version", &RDelta::get_version)
    .method("get_all_debug", &RDelta::get_all_debug)
    ;
}