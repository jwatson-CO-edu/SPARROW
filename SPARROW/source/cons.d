module cons;

///// Getters and Setters ////////////////////////

// Basic Getters //
Atom* get_car( Atom* atm ){  return atm.car;  } // ---------------------- Get left  pair item
Atom* get_cdr( Atom* atm ){  return atm.cdr;  } // ---------------------- Get right pair item
Atom* first(   Atom* atm ){  return get_car(atm);  } // ----------------- Return the first item of an LS pair
Atom* second(  Atom* atm ){  return get_car(get_cdr(atm));  } // -------- Return the second item in a list, (cadr l)
Atom* third(   Atom* atm ){  return get_car(get_cdr(get_cdr(atm)));  } // Return the third item of 'l', (caddr l)

// Aliased Getters //
Atom* condLinesOf( Atom* atm ){  return get_cdr( atm );  }
Atom* formLinesOf( Atom* atm ){  return get_cdr( atm );  }
Atom* argsOf(      Atom* atm ){  return get_cdr( atm );  }
// Atom* paramsOf(    Atom* atm ){  return get_cdr( atm );  }
// Atom* tableOf(     Atom* atm ){  return first( atm );    }
Atom* nameOf(      Atom* atm ){  return first( atm );    }
Atom* questionOf(  Atom* atm ){  return first( atm );    }
Atom* textOf(      Atom* atm ){  return second( atm );   }
Atom* formalsOf(   Atom* atm ){  return first( atm );    }
Atom* answerOf(    Atom* atm ){  return second( atm );   }
Atom* bodyOf(      Atom* atm ){  return second( atm );   }


// Basic Setters //

bool set_car_B( Atom* atm, Atom* carAtm ){  
    // Set left  pair item
    atm.car = carAtm;  
    return true;  
} 


bool set_cdr_B( Atom* atm, Atom* cdrAtm ){  
    // Set right pair item
    atm.cdr = cdrAtm;  
    return true;  
} 


////////// LIST PROCESSING /////////////////////////////////////////////////////////////////////////


///// Accessing & Constructing ///////////////////

Atom* consify_atom( Atom* carAtm ){
    // Wrap the `atm` in a cons, with `carAtm` as 'car'
    return new Atom( carAtm, empty_atom() );
}


Atom* find_terminus( Atom* list ){
    // Iterate to the ending cons of the list and return a pointer to it
    // 0. Set the argument equal to our pointer
    Atom* curr = list;
    // 1. If this is a cons structure, then we must find the end of it
    if( list.kind == F_Type.CONS ){
        // 2. Iterate pointer to next `cdr` until we reach a pair that contains the terminating null, return pair
        while( !p_empty( curr.cdr ) ){  curr = curr.cdr;  }
        // while( !p_empty( curr ) ){  curr = curr.cdr;  }
        return curr;
    }else{ // Else atom was literal, it is its own terminus
        return list;
    }
}


Atom* append( Atom* list, Atom* atm = null ){
    // Append an atom to the end of a conslist, Create a conslist if none exists, return pointer to list head
    Atom* rtnLst = null;
    Atom* endCns = null;
    // 1. If the given list is a cons list, it is either an empty cons or the head of a LISP list
    if( list.kind == F_Type.CONS ){
        /* 2. If we were given an atom to append, it either belongs in the `car` of the empty cons,
              or in the `car` of a new terminal cons */
        if( p_empty( list.car ) ){
            set_car_B( list, atm );
        }else{
            endCns = find_terminus( list );
            set_cdr_B( endCns, consify_atom( atm ) );
        }
        rtnLst = list;
    // 3. Else we either have one or two non-cons atoms
    }else{ 
        rtnLst = consify_atom( list ); // --------------- ( `list` , [X] )
        if( !p_empty( atm ) ){
            set_cdr_B( rtnLst, consify_atom( atm ) ); // ( `list` , ( `atom` , [X] ) )
        }
    }
    return rtnLst;
}

Atom* make_list_of_2( Atom* atm1, Atom* atm2 ){
    // return a two-item list with 's1' as the first item and 's2' as the second item
    Atom* second = new Atom( atm2, empty_atom() );
    return new Atom( atm1, second );
}


////////// INTERPRETATION && EXECUTION /////////////////////////////////////////////////////////////

///// Scheme --to-> D ////////////////////////////

double[] flatten_double_list( Atom* dbblList ){
    // Take a LISP list of numbers and convert to a Dlang dyn. array
    Atom*    currCons = dbblList;
    double[] rtnArr;
    while( !p_empty( currCons ) ){
        if( p_number( currCons.car ) ){  rtnArr ~= currCons.car.num;  }
        currCons = currCons.cdr;
    }
    return rtnArr;
}


string[] flatten_string_list( Atom* strnList ){
    // Take a LISP list of strings and convert to a Dlang dyn. array
    Atom*    currCons = strnList;
    string[] rtnArr;
    while( !p_empty( currCons ) ){
        if( p_string( currCons.car ) ){  rtnArr ~= currCons.car.str;  }
        currCons = currCons.cdr;
    }
    return rtnArr;
}


Atom*[] flatten_atom_list( Atom* atomList ){
    // Take a LISP list of Atoms and convert to a Dlang dyn. array
    Atom*   currCons = atomList;
    Atom*[] rtnArr;
    while( !p_empty( currCons ) ){
        rtnArr ~= currCons.car;
        currCons = currCons.cdr;
    }
    return rtnArr;
}


Atom*[] flatten_cons_list( Atom* atomList ){
    // Take a LISP list of Atoms and convert to a Dlang dyn. array
    Atom*   currCons = atomList;
    Atom*[] rtnArr;
    ulong   depth = 0;
    while( !p_empty( currCons ) ){
        rtnArr ~= currCons.car;
        currCons = currCons.cdr;
    }
    return rtnArr;
}