#ifndef DEMO2400_H
#define DEMO2400_H

/*
demo2400_helpers.hpp
James Watson, 2020-10
Helper functions for CSCI 2400 @ CU Boulder
*/

#include <iostream> // cout
#include <string> // --- string manipulation
#include <cmath> // ---- abs, min/max, trig, hyperbolic, power, exp, error, rounding
#include <limits> // --- Infinity
#include <stdlib.h> // - malloc, srand , rand , atof , strtof
#include <time.h> // --- time , for getting sys time and seeding random numbers
#include <ctime> // ---- time , for date formatting
#include <chrono> // more different time 
#include <limits> // --- number limits of data types, limit on 'cin.ignore'
#include <iostream> // - standard input and output , istream } Input / Output
#include <fstream> // -- File I/O                           /
#include <sstream> // -- Text streams    
#include <vector>  
#include <sys/stat.h> 
#include <experimental/filesystem> 
// #include <filesystem> 

// namespace fs = std::filesystem;
namespace fs = std::experimental::filesystem;

typedef  std::chrono::steady_clock::time_point  StdTime;
typedef  std::chrono::steady_clock              StdClock;

using std::chrono::duration_cast; 
using std::chrono::microseconds;
using std::cout;
using std::endl;
using std::string;
using std::vector;
using std::ostream;
using std::flush;
using std::vector;
using std::getline;

// print the binary representation of a uint
void print_binary_int( unsigned int arg ); 

// Print the hexadecimal representation of a uint
void print_hex_int( unsigned int a );

// print the binary representation of a uchar
void print_binary_char( unsigned char arg );

void random_seed_RUN_ONCE_MAIN(); // init random

double rand_01(); // Return a random number on [0,1)

double time_elapsed( StdTime& clok ); // Get the elapsed time

void fill_array_rand( double arr[] , size_t len ); // Populate an array with random doubles on [0,1)

string yes_no( bool expr ); // Return an affirmative string for true -or- a negative string for false

ostream& operator<<( ostream& os , const std::vector<u_char>& vec );

bool check_exist( string path , string check );

vector<string> split( string s , char sep );

vector<string> list_path( string path );

bool has_substr( string& superStr , string& subStr );

void print_args( int argc, char *argv[] );

template<typename T> // NOTE: Templated functions must have their definition in the header file
ostream& operator<<( ostream& os , const std::vector<T>& vec ) { // ostream '<<' operator for vectors
    // NOTE: This function assumes that the ostream '<<' operator for T has already been defined
    os << "[ ";
    for (size_t i = 0; i < vec.size(); i++) {
        os << vec[i];
        if (i + 1 < vec.size()) { os << ", "; }
    }
    os << " ]";
    return os; // You must return a reference to the stream!
}

template<typename T> // NOTE: Templated functions must have their definition in the header file
bool is_arg_in_vector( T arg , const std::vector<T>& vec ){
    // Return true if 'arg' is in 'st' , false otherwise
    // URL , resolve dependent templated typenames: https://stackoverflow.com/a/11275548
    // URL , const_iterator: https://stackoverflow.com/a/309589
    typename std::vector<T>::const_iterator it = find( vec.begin() , vec.end() , arg ); 
    return it != vec.end();
}

template<typename T>
T randrange( T lo , T hi ){ return (T) std::min( lo , hi ) + (T) rand_01() * abs( hi - lo ); }

/// END ///////////////////////////////////////////////////////////////////////////////////////////////////////////
#endif