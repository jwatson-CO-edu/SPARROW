#ifndef CHIP8_H
#define CHIP8_H

#include <cstdint>
#include <cmath>
#include <stack>

////////// CHIP 8 PROPERTIES //////////////////////////////////////////////////////////////////////
// URL:  https://tobiasvl.github.io/blog/write-a-chip-8-emulator/

///// System Properties & Sizes /////

#define _RAM_SIZE 4096 // Number of addresses in RAM
#define _DSP_WDTH   64 // Width of monochrome display
#define _DSP_HGHT   32 // Height of monochrome display
#define _REG_VARI   16 // Number of variable registers

///// Shortcuts & Aliases ///// 
typedef uint8_t  Word; //- CPU word
typedef uint16_t Dubw;  // Double words
typedef uint16_t Reg16; // 16bit register
typedef uint8_t  Reg8; //-  8bit register
typedef u_char   Byte; //-  8bits

using std::stack;

////////// CHIP 8 INTERPRETER /////////////////////////////////////////////////////////////////////
// URL:  https://tobiasvl.github.io/blog/write-a-chip-8-emulator/

class CHIP8{
public:


///// Components ////////////////////////////////

// Memory: CHIP-8 has direct access to up to 4 kilobytes of RAM
Word RAM[_RAM_SIZE];

// Display: 64 x 32 pixels (or 128 x 64 for SUPER-CHIP) monochrome, ie. black or white
bool display[_DSP_WDTH][_DSP_HGHT];

// A program counter, often called just “PC”, which points at the current instruction in memory
Reg16 PC;

// One 16-bit index register called “I” which is used to point at locations in memory
Reg16 I;

// A stack for 16-bit addresses, which is used to call subroutines/functions and return from them
stack<Dubw> S;

// An 8-bit delay timer which is decremented at a rate of 60 Hz (60 times per second) until it reaches 0
Reg8 delay;

// An 8-bit sound timer which functions like the delay timer, but which also gives off a beeping sound as long as it’s not 0
Reg8 sound;

// 16 8-bit (one byte) general-purpose variable registers numbered 0 - F, ie. 0 through 15 in decimal, called V0 through `VF
Reg8 VX[_REG_VARI];

///// Utilities /////////////////////////////////
inline bool UT_incr_PC(){
    // Increment the Program Counter
    PC += 2;
    return 0;
}

inline Reg8 UT_VX_contents( Byte x ){
    // Fetch contents of the `VX` register
    return VX[ x ];
}

inline bool UT_VX_equal( Byte x, Word nn ){
    // Return 1 if the contents of register VX are equal to `NN`, Otherwise return 0
    return UT_VX_contents( x ) == nn;
}

inline bool UT_VX_VY_equal( Byte x, Byte y ){
    // Return 1 if the contents of register VX are equal to `NN`, Otherwise return 0
    return UT_VX_contents( x ) == UT_VX_contents( y );
}

///// OpCodes ///////////////////////////////////
inline bool OP_clear_screen(){
    // Set all screen bits to zero
    for( u_char i = 0; i < _DSP_WDTH; ++i ){
        for( u_char j = 0; j < _DSP_HGHT; ++j ){
            display[i][j] = 0;
        }
    }
    return 0;
}

inline bool OP_jump( Dubw nnn ){
    // Unconditional jump to a location in RAM
    PC = nnn;
    return 0;
}

inline bool OP_call( Dubw nnn ){
    /* calls the subroutine at memory location NNN. just like 1NNN, you should set PC to NNN. 
       However, the difference between a jump and a call is that this instruction should 
       first should push the current PC to the stack, so the subroutine can return later. */
    S.push( PC );
    OP_jump( nnn );
    return 0;
}

inline bool OP_return(){
    PC = S.top();
    S.pop();
    return 0;
}

inline bool OP_skip_VX_equal_value( Byte x, Word nn ){
    // skip one instruction if the value in VX is equal to NN
    if( UT_VX_equal( x, nn ) ){  UT_incr_PC();  }
    return 0;
}

inline bool OP_skip_VX_not_equal_value( Byte x, Word nn ){
    // skip one instruction if the value in VX is equal to NN
    if( !UT_VX_equal( x, nn ) ){  UT_incr_PC();  }
    return 0;
}

inline bool OP_skip_VX_VY_equal( Byte x, Byte y ){
    if( UT_VX_VY_equal( x, y ) ){  UT_incr_PC();  }
    return 0;
}

inline bool OP_skip_VX_VY_not_equal( Byte x, Byte y ){
    if( !UT_VX_VY_equal( x, y ) ){  UT_incr_PC();  }
    return 0;
}

inline bool OP_set_VX( Byte x, Word nn ){
    // Set the contents of register VX equal to `NN`
    VX[ x ] = nn;
    return 0;
}

inline bool OP_add_to_VX( Byte x, Word nn ){
    // Accumulate `NN` into register VX
    // NOTE: Carry bit NOT set by this op!
    VX[ x ] += nn;
    return 0;
}

inline bool OP_set_VX_to_VY( Byte x, Byte y ){
    VX[ x ] = VX[ y ];
    return 0;
}

///// Engine ////////////////////////////////////

void decrement_timers(){
    // Decrement the two 8bit timers as needed, (This does not impact the Fetch-Decode-Exec loop)
    delay || delay-- ; // Decrement the delay timer if it is not zero
    sound || sound-- ; // Decrement the sound timer if it is not zero
}

Dubw fetch(){
    // read two successive bytes from memory and combine them into one 16-bit instruction.
    UT_incr_PC();
    return *(RAM + PC) << 8 + *(RAM + (PC + 1));
}

bool fetch_decode_exec(){
    // An emulator’s main task is simple. It runs in an infinite loop, and does these three tasks in succession:
    
    // 1. Fetch the instruction from memory at the current PC (program counter)
    Dubw instruction = fetch();

    Byte T   = (instruction & 0xF000) >> 12; // 1st Nybble
    Byte X   = (instruction & 0x0F00) >>  8; // 2nd Nybble
    Byte Y   = (instruction & 0x00F0) >>  4; // 3rd Nybble
    Byte N   = (instruction & 0x000F); // ----- 4th Nybble
    Byte NN  = (instruction & 0x00FF); // ----- 3rd & 4th Nybble
    Dubw NNN = (instruction & 0x0FFF); // ----- 2nd & 3rd & 4th Nybble

    // 2. Decode the instruction to find out what the emulator should do
    // 3. Execute the instruction and do what it tells you

    //// Operations without Arguments ////
    switch( instruction ){

        /// Clear Screen ///
        case 0x00E0:
            OP_clear_screen();
            break;

        /// Return ///
        case 0x00EE:
            OP_return();
            break;

        /// Else Op has arguments, break ///
        default:
            break;

    }

    

    //// Operations with Arguments ////
    switch( T ){

        /// Machine Sub ///
        case 0x0:
            // NOT USED: pause execution of the CHIP-8 and call a sub written in machine language at address NNN instead.
            break;

        /// Jump ///
        case 0x1:
            OP_jump( NNN );
            break;

        /// Call ///
        case 0x2:
            OP_call( NNN );
            break;
        
        /// Skip VX Equal Value ///
        case 0x3:
            OP_skip_VX_equal_value( X, NN );
            break;

        /// Skip VX Not Equal Value ///
        case 0x4:
            OP_skip_VX_not_equal_value( X, NN );
            break;

        /// Skip VX Equal VY ///
        case 0x5:
            OP_skip_VX_VY_equal( X, Y );
            break;

        /// Set VX to Value ///
        case 0x6:
            OP_set_VX( X, NN );
            break;

        /// Add Value to VX ///
        case 0x7:
            OP_add_to_VX( X, NN );
            break;

        /// Arithemetic & Logic ///
        case 0x8:

            switch( N ){

                // Set //
                case 0x0:
                    break;

                // Logical OR //
                case 0x1:
                    break;

                // Logical AND //
                case 0x2:
                    break;

                // Logical XOR //
                case 0x3:
                    break;

                // Subtract VX-VY //
                case 0x4:
                    break;

                // Subtract VY-VX //
                case 0x6:
                    break;

                // Shift Right //
                case 0x7:
                    break;

                // Shift Left //
                case 0xE:
                    break;

                // NO-OP //
                default:
                    break;
            }

            break;

        /// Skip VX Not Equal VY ///
        case 0x9:
            OP_skip_VX_VY_not_equal( X, Y );
            break;

        /// Set Index ///
        case 0xA:
            break;

        /// Jump with Offset ///
        case 0xB:
            break;

        /// Random ///
        case 0xC:
            break;

        /// Draw ///
        case 0xD:
            break;

        /// Skip if Keydown ///
        case 0xE:
            break;

        /// Timer Manipulation ///
        case 0xF:
            break;

        /// NO-OP ///
        default:
            break;
    }

}



};

#endif