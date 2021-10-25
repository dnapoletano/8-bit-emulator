#include "CPU.hpp"
#include "Bus.hpp"

#include <iostream>
#include <string>

CPU::CPU()
: Accumulator{0x00}, XRegister{0x00}, YRegister{0x00},
  StackPointer{0x00}, ProgramCounter{0x0000}, StatusRegister{0x00},
  Fetched{0x00}, AbsoluteAddress{0x0000}, RelativeAddress{0x0000},
  OpCode{0x00}, Cycles{0}, ClockCount{0}
{
	// Assembles the translation table. It's big, it's ugly, but it yields a convenient way
	// to emulate the 6502. I'm certain there are some "code-golf" strategies to reduce this
	// but I've deliberately kept it verbose for study and alteration

	// It is 16x16 entries. This gives 256 instructions. It is arranged to that the bottom
	// 4 bits of the instruction choose the column, and the top 4 bits choose the row.

	// For convenience to get function pointers to members of this class, I'm using this
	// or else it will be much much larger :D

	// The table is one big initialiser list of initialiser lists...
  using a = CPU;
  Lookup =
    {
      { "BRK", &a::BRK, &a::IMM, 7 },{ "ORA", &a::ORA, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::ZP0, 3 },{ "ASL", &a::ASL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHP", &a::PHP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::IMM, 2 },{ "ASL", &a::ASL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABS, 4 },{ "ASL", &a::ASL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
      { "BPL", &a::BPL, &a::REL, 2 },{ "ORA", &a::ORA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ZPX, 4 },{ "ASL", &a::ASL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLC", &a::CLC, &a::IMP, 2 },{ "ORA", &a::ORA, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABX, 4 },{ "ASL", &a::ASL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
      { "JSR", &a::JSR, &a::ABS, 6 },{ "AND", &a::AND, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "BIT", &a::BIT, &a::ZP0, 3 },{ "AND", &a::AND, &a::ZP0, 3 },{ "ROL", &a::ROL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLP", &a::PLP, &a::IMP, 4 },{ "AND", &a::AND, &a::IMM, 2 },{ "ROL", &a::ROL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "BIT", &a::BIT, &a::ABS, 4 },{ "AND", &a::AND, &a::ABS, 4 },{ "ROL", &a::ROL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
      { "BMI", &a::BMI, &a::REL, 2 },{ "AND", &a::AND, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ZPX, 4 },{ "ROL", &a::ROL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEC", &a::SEC, &a::IMP, 2 },{ "AND", &a::AND, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ABX, 4 },{ "ROL", &a::ROL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
      { "RTI", &a::RTI, &a::IMP, 6 },{ "EOR", &a::EOR, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "EOR", &a::EOR, &a::ZP0, 3 },{ "LSR", &a::LSR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHA", &a::PHA, &a::IMP, 3 },{ "EOR", &a::EOR, &a::IMM, 2 },{ "LSR", &a::LSR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::ABS, 3 },{ "EOR", &a::EOR, &a::ABS, 4 },{ "LSR", &a::LSR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
      { "BVC", &a::BVC, &a::REL, 2 },{ "EOR", &a::EOR, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ZPX, 4 },{ "LSR", &a::LSR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLI", &a::CLI, &a::IMP, 2 },{ "EOR", &a::EOR, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ABX, 4 },{ "LSR", &a::LSR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
      { "RTS", &a::RTS, &a::IMP, 6 },{ "ADC", &a::ADC, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ADC", &a::ADC, &a::ZP0, 3 },{ "ROR", &a::ROR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLA", &a::PLA, &a::IMP, 4 },{ "ADC", &a::ADC, &a::IMM, 2 },{ "ROR", &a::ROR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::IND, 5 },{ "ADC", &a::ADC, &a::ABS, 4 },{ "ROR", &a::ROR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
      { "BVS", &a::BVS, &a::REL, 2 },{ "ADC", &a::ADC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ZPX, 4 },{ "ROR", &a::ROR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEI", &a::SEI, &a::IMP, 2 },{ "ADC", &a::ADC, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ABX, 4 },{ "ROR", &a::ROR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
      { "???", &a::NOP, &a::IMP, 2 },{ "STA", &a::STA, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZP0, 3 },{ "STA", &a::STA, &a::ZP0, 3 },{ "STX", &a::STX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "DEY", &a::DEY, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 2 },{ "TXA", &a::TXA, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "STY", &a::STY, &a::ABS, 4 },{ "STA", &a::STA, &a::ABS, 4 },{ "STX", &a::STX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
      { "BCC", &a::BCC, &a::REL, 2 },{ "STA", &a::STA, &a::IZY, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZPX, 4 },{ "STA", &a::STA, &a::ZPX, 4 },{ "STX", &a::STX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "TYA", &a::TYA, &a::IMP, 2 },{ "STA", &a::STA, &a::ABY, 5 },{ "TXS", &a::TXS, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::NOP, &a::IMP, 5 },{ "STA", &a::STA, &a::ABX, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::XXX, &a::IMP, 5 },
      { "LDY", &a::LDY, &a::IMM, 2 },{ "LDA", &a::LDA, &a::IZX, 6 },{ "LDX", &a::LDX, &a::IMM, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "LDY", &a::LDY, &a::ZP0, 3 },{ "LDA", &a::LDA, &a::ZP0, 3 },{ "LDX", &a::LDX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "TAY", &a::TAY, &a::IMP, 2 },{ "LDA", &a::LDA, &a::IMM, 2 },{ "TAX", &a::TAX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "LDY", &a::LDY, &a::ABS, 4 },{ "LDA", &a::LDA, &a::ABS, 4 },{ "LDX", &a::LDX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
      { "BCS", &a::BCS, &a::REL, 2 },{ "LDA", &a::LDA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "LDY", &a::LDY, &a::ZPX, 4 },{ "LDA", &a::LDA, &a::ZPX, 4 },{ "LDX", &a::LDX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "CLV", &a::CLV, &a::IMP, 2 },{ "LDA", &a::LDA, &a::ABY, 4 },{ "TSX", &a::TSX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 4 },{ "LDY", &a::LDY, &a::ABX, 4 },{ "LDA", &a::LDA, &a::ABX, 4 },{ "LDX", &a::LDX, &a::ABY, 4 },{ "???", &a::XXX, &a::IMP, 4 },
      { "CPY", &a::CPY, &a::IMM, 2 },{ "CMP", &a::CMP, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPY", &a::CPY, &a::ZP0, 3 },{ "CMP", &a::CMP, &a::ZP0, 3 },{ "DEC", &a::DEC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INY", &a::INY, &a::IMP, 2 },{ "CMP", &a::CMP, &a::IMM, 2 },{ "DEX", &a::DEX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "CPY", &a::CPY, &a::ABS, 4 },{ "CMP", &a::CMP, &a::ABS, 4 },{ "DEC", &a::DEC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
      { "BNE", &a::BNE, &a::REL, 2 },{ "CMP", &a::CMP, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ZPX, 4 },{ "DEC", &a::DEC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLD", &a::CLD, &a::IMP, 2 },{ "CMP", &a::CMP, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ABX, 4 },{ "DEC", &a::DEC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
      { "CPX", &a::CPX, &a::IMM, 2 },{ "SBC", &a::SBC, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPX", &a::CPX, &a::ZP0, 3 },{ "SBC", &a::SBC, &a::ZP0, 3 },{ "INC", &a::INC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INX", &a::INX, &a::IMP, 2 },{ "SBC", &a::SBC, &a::IMM, 2 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::SBC, &a::IMP, 2 },{ "CPX", &a::CPX, &a::ABS, 4 },{ "SBC", &a::SBC, &a::ABS, 4 },{ "INC", &a::INC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
      { "BEQ", &a::BEQ, &a::REL, 2 },{ "SBC", &a::SBC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ZPX, 4 },{ "INC", &a::INC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SED", &a::SED, &a::IMP, 2 },{ "SBC", &a::SBC, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ABX, 4 },{ "INC", &a::INC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
    };
}

CPU::~CPU()
{

}

/// Addressing modes, they essentially set the Absolute and Relative Addresses.
/// These functions return whether or not additional clock cycles
/// are required.

/// Implied: the address of the operand is implicititly stated in the OC
/// of the instruction (the accumulator)
uint8_t CPU::IMP()
{
  Fetched = Accumulator;
  return 0;
}

/// Immediate: the address of the operand is in the second byte of the
/// instruction
uint8_t CPU::IMM()
{
  AbsoluteAddress = ProgramCounter++;
  return 0;
}

/// Zero page:
/// absolute address a location in the first 0xFF bytes of address range,
/// thus it requires only one byte (and one less clock cycle)
uint8_t CPU::ZP0()
{
  /// read and increment the pc
  AbsoluteAddress = Read(ProgramCounter++);
  /// The address is made of two bytes: hi and low,
  /// say we have:
  /// 0000 1010, as we know we are in the zero page,
  /// we need to extract the lower one, which can be done
  /// x = x & 0b00001111, which is equal to x &= 0x00FF
  AbsoluteAddress &= 0x00FF;
  return 0;
}

/// Zero page with offset x-register:
/// absolute address a location in the first 0xFF bytes of address range,
/// thus it requires only one byte (and one less clock cycle)
uint8_t CPU::ZPX()
{
  AbsoluteAddress = Read(ProgramCounter++) + XRegister;
  AbsoluteAddress &= 0x00FF;
  return 0;
}

uint8_t CPU::ZPY()
{
  AbsoluteAddress = Read(ProgramCounter++) + YRegister;
  AbsoluteAddress &= 0x00FF;
  return 0;
}

/// Relative mode, can only access a maximum
/// range of -128, +127 from the current position
uint8_t CPU::REL()
{
  RelativeAddress = Read(ProgramCounter++);
  /// if the 7-th bit of Relative Address is set
  /// (the relative address is a negative number), I construct
  /// a 16 bit number with all bits of the high byte set, such
  /// that arithmetics works out when summing this with the PC.
  if(RelativeAddress & 0x80){
    RelativeAddress |= 0xFF00;
  }
  return 0;
}

/// Absolute: load a full 16bit address and use it
uint8_t CPU::ABS()
{
  /// little-endian, so read low byte first
  uint8_t low {Read(ProgramCounter++)};
  uint8_t hi  {Read(ProgramCounter++)};
  /// push the high byte to the left and combine with low byte.
  AbsoluteAddress = (hi << 8) | low;
  return 0;
}

/// Absolute with X-Offset
uint8_t CPU::ABX()
{
  /// little-endian, so read low byte first
  uint8_t low {Read(ProgramCounter++)};
  uint8_t hi  {Read(ProgramCounter++)};
  /// push the high byte to the left and combine with low byte.
  AbsoluteAddress = (hi << 8) | low;
  AbsoluteAddress += XRegister;
  /// candidate for incrementing the clock cycles if address crosses
  /// the page boundary: to check this we check that the high part
  /// of the address, which we get by masking it with 0xFF00, must be equal
  /// to the high byte which determines the page
  if((AbsoluteAddress & 0xFF00) != (hi << 8)){
    return 1;
  }

  return 0;
}

uint8_t CPU::ABY()
{
  uint8_t low {Read(ProgramCounter++)};
  uint8_t hi  {Read(ProgramCounter++)};
  /// push the high byte to the left and combine with low byte.
  AbsoluteAddress = (hi << 8) | low;
  AbsoluteAddress += YRegister;
  /// candidate for incrementing the clock cycles if address crosses
  /// the page boundary: to check this we check that the high part
  /// of the address, which we get by masking it with 0xFF00, must be equal
  /// to the high byte which determines the page
  if((AbsoluteAddress & 0xFF00) != (hi << 8)){
    return 1;
  }
  return 0;
}

/// Indirect modes, this is the implementation
/// of pointers, there is a bug in the hardware which need however
/// to be emulated. If the low byte of the
/// supplied address is 0xFF, then to read the high byte of the actual address
/// we need to cross a page boundary. This doesnt actually work on the chip as
/// designed, instead it wraps back around in the same page, yielding an
/// invalid actual address
uint8_t CPU::IND()
{
  uint16_t PointerLow  {Read(ProgramCounter++)};
  uint16_t PointerHigh {Read(ProgramCounter++)};
  uint16_t Pointer = (PointerHigh << 8 )| PointerLow;
  /// Bug:
  if(PointerLow & 0x00FF){
    AbsoluteAddress = (Read((Pointer & 0x00FF ))<< 8)
      | Read(Pointer + 0);
  } else { /// Normal behaviour
    AbsoluteAddress = Read(Pointer + 1) << 8 | Read(Pointer + 0);
  }
  return 0;
}
/// Address Mode: Indirect X
/// The supplied 8-bit address is offset by X Register to index
/// a location in page 0x00. The actual 16-bit address is read
/// from this location
uint8_t CPU::IZX()
{
  uint16_t temp {Read(ProgramCounter++)};
  /// Now read the address from the given address
  uint16_t ByteLow {Read((temp + (uint16_t) XRegister) & 0x00FF)};
  uint16_t ByteHi {Read((temp + (uint16_t) XRegister + 1) & 0x00FF)};
  AbsoluteAddress = (ByteHi << 8) | ByteLow;
  return 0;
}

uint8_t CPU::IZY()
{
  uint16_t temp {Read(ProgramCounter++)};
  /// Now read the address from the given address
  uint16_t ByteLow {Read(temp & 0x00FF)};
  uint16_t ByteHi {Read((temp + 1) & 0x00FF)};
  AbsoluteAddress = ((ByteHi << 8) | ByteLow) + YRegister;
  if((AbsoluteAddress & 0x00FF) != (ByteHi << 8)){
    return 1;
  }
  return 0;
}
////////////////////////////////////////////////////////////////////////
/// Instructions
////////////////////////////////////////////////////////////////////////
/// Add with Carry
/// Instruction: Add with Carry In
/// Function:    A = A + M + C
/// Flags Out:   C, V, N, Z
///
/// Explanation:
/// The purpose of this function is to add a value to the accumulator and a carry bit. If
/// the result is > 255 there is an overflow setting the carry bit. Ths allows you to
/// chain together ADC instructions to add numbers larger than 8-bits. This in itself is
/// simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow.
///
/// 10000100 = 128 + 4 = 132 in normal circumstances, we know this as unsigned and it allows
/// us to represent numbers between 0 and 255 (given 8 bits). The 6502 can also interpret
/// this word as something else if we assume those 8 bits represent the range -128 to +127,
/// i.e. it has become signed.
///
/// Since 132 > 127, it effectively wraps around, through -128, to -124. This wraparound is
/// called overflow, and this is a useful to know as it indicates that the calculation has
/// gone outside the permissable range, and therefore no longer makes numeric sense.
///
/// Note the implementation of ADD is the same in binary, this is just about how the numbers
/// are represented, so the word 10000100 can be both -124 and 132 depending upon the
/// context the programming is using it in. We can prove this!
///
///  10000100 =  132  or  -124
/// +00010001 = + 17      + 17
///  ========    ===       ===     See, both are valid additions, but our interpretation of
///  10010101 =  149  or  -107     the context changes the value, not the hardware!
///
/// In principle under the -128 to 127 range:
/// 10000000 = -128, 11111111 = -1, 00000000 = 0, 00000000 = +1, 01111111 = +127
/// therefore negative numbers have the most significant set, positive numbers do not
///
/// To assist us, the 6502 can set the overflow flag, if the result of the addition has
/// wrapped around. V <- ~(A^M) & A^(A+M+C) :D lol, let's work out why!
///
/// Let's suppose we have A = 30, M = 10 and C = 0
///          A = 30 = 00011110
///          M = 10 = 00001010+
///     RESULT = 40 = 00101000
///
/// Here we have not gone out of range. The resulting significant bit has not changed.
/// So let's make a truth table to understand when overflow has occurred. Here I take
/// the MSB of each component, where R is RESULT.
///
/// A  M  R | V | A^R | A^M |~(A^M) |
/// 0  0  0 | 0 |  0  |  0  |   1   |
/// 0  0  1 | 1 |  1  |  0  |   1   |
/// 0  1  0 | 0 |  0  |  1  |   0   |
/// 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
/// 1  0  0 | 0 |  1  |  1  |   0   |
/// 1  0  1 | 0 |  0  |  1  |   0   |
/// 1  1  0 | 1 |  1  |  0  |   1   |
/// 1  1  1 | 0 |  0  |  0  |   1   |
///
/// We can see how the above equation calculates V, based on A, M and R. V was chosen
/// based on the following hypothesis:
///       Positive Number + Positive Number = Negative Result -> Overflow
///       Negative Number + Negative Number = Positive Result -> Overflow
///       Positive Number + Negative Number = Either Result -> Cannot Overflow
///       Positive Number + Positive Number = Positive Result -> OK! No Overflow
///       Negative Number + Negative Number = Negative Result -> OK! NO Overflow
uint8_t CPU::ADC()
{
  FetchData();
  uint16_t temp  = (uint16_t) Accumulator + (uint16_t) Fetched
    + (uint16_t) GetFlag(Carry);
  /// Set the carry flag if the number surpasses +127 (or 255)
  SetFlag(Carry,(temp > 255));
  /// Set the zero flag if the result is zero
  SetFlag(Zero, (temp & 0x00FF) == 0);
  /// Set the overflow flag if summing two positive numbers results in anegative one
  /// or the opposite case
  SetFlag(OverFlow, ((~(uint16_t)Accumulator ^ (uint16_t) Fetched) &
    ((uint16_t)Accumulator ^ (uint16_t) temp)) & 0x0080);
  /// check if the most significant bit is set
  SetFlag(Negative, (temp & 0x80));
  /// the accumulator is 8bit!
  Accumulator = temp & 0x00FF;
  return 1;
}

/// Instruction: Bitwise Logic AND
/// Function:    A = A & M
/// Flags Out:   N, Z
uint8_t CPU::AND()
{
  FetchData();
  Accumulator = Accumulator & Fetched;
  SetFlag(Zero, Accumulator == 0x00);
  SetFlag(Negative, Accumulator & 0x80);
  return 1;
}

/// Arithmetic Shift Left
/// A = C <- (A << 1) <- 0
/// Sets N,Z and C flags
uint8_t CPU::ASL()
{
  FetchData();
  uint16_t temp = (uint16_t) Fetched << 1;
  /// set Carry if the high byte is > 0
  SetFlag(Carry, (temp & 0xFF00) > 0);
  /// set Zero flag if the low byte is zero
  SetFlag(Zero, (temp & 0x00FF) == 0x00);
  /// set negative if last bit of low byte is set
  SetFlag(Negative, (temp & 0x0080));

  /// If the addressing mode is implied, directly set
  /// the accumulator (which is only 8-bit!)
  if(Lookup[OpCode].AddressMode == &CPU::IMP){
    Accumulator = temp & 0x00FF;
  } else { /// write to the right address
    Write(AbsoluteAddress, temp & 0x00FF);
  }
  return 0;
}

/// Branch if carry clear
uint8_t CPU::BCC()
{
  if(GetFlag(Carry) == 0){
    ++Cycles;
    AbsoluteAddress = ProgramCounter + RelativeAddress;
    if((AbsoluteAddress & 0xFF00) != (ProgramCounter & 0xFF00)){
      ++Cycles;
    }
    ProgramCounter = AbsoluteAddress;
  }
  return 0;
}

/// Branch if carry set
uint8_t CPU::BCS()
{
  if(GetFlag(Carry) == 1){
    ++Cycles;
    AbsoluteAddress = ProgramCounter + RelativeAddress;
    if((AbsoluteAddress & 0xFF00) != (ProgramCounter & 0xFF00)){
      ++Cycles;
    }
    ProgramCounter = AbsoluteAddress;
  }
  return 0;
}

/// Branch if equal
uint8_t CPU::BEQ()
{
  if(GetFlag(Zero) == 1){
    ++Cycles;
    AbsoluteAddress = ProgramCounter + RelativeAddress;
    if((AbsoluteAddress & 0xFF00) != (ProgramCounter & 0xFF00)){
      ++Cycles;
    }
    ProgramCounter = AbsoluteAddress;
  }
  return 0;
}

/// Tests bits in memory with the accumulator
uint8_t CPU::BIT()
{
  FetchData();
  uint16_t temp = Fetched & Accumulator;
  SetFlag(Negative, Fetched & (1 << 7));
  SetFlag(OverFlow, Fetched & (1 << 6));
  SetFlag(Zero, (temp & 0x00FF) == 0x00);

  return 0;
}

/// Branch if negative
uint8_t CPU::BMI()
{
  if(GetFlag(Negative) == 1){
    ++Cycles;
    AbsoluteAddress = ProgramCounter + RelativeAddress;
    if((AbsoluteAddress & 0xFF00) != (ProgramCounter & 0xFF00)){
      ++Cycles;
    }
    ProgramCounter = AbsoluteAddress;
  }
  return 0;
}

/// Branch not equal
uint8_t CPU::BNE()
{
  if(GetFlag(Zero) == 0){
    ++Cycles;
    AbsoluteAddress = ProgramCounter + RelativeAddress;
    if((AbsoluteAddress & 0xFF00) != (ProgramCounter & 0xFF00)){
      ++Cycles;
    }
    ProgramCounter = AbsoluteAddress;
  }
  return 0;
}

/// Branch if positive
uint8_t CPU::BPL()
{
  if(GetFlag(Negative) == 0){
    ++Cycles;
    AbsoluteAddress = ProgramCounter + RelativeAddress;
    if((AbsoluteAddress & 0xFF00) != (ProgramCounter & 0xFF00)){
      ++Cycles;
    }
    ProgramCounter = AbsoluteAddress;
  }
  return 0;
}

uint8_t CPU::BRK()
{
 return 0;
}

/// ranch if overflow clear
uint8_t CPU::BVC()
{
  if(GetFlag(OverFlow) == 0){
    ++Cycles;
    AbsoluteAddress = ProgramCounter + RelativeAddress;
    if((AbsoluteAddress & 0xFF00) != (ProgramCounter & 0xFF00)){
      ++Cycles;
    }
    ProgramCounter = AbsoluteAddress;
  }
  return 0;
}

uint8_t CPU::BVS()
{
  if(GetFlag(OverFlow) == 1){
    ++Cycles;
    AbsoluteAddress = ProgramCounter + RelativeAddress;
    if((AbsoluteAddress & 0xFF00) != (ProgramCounter & 0xFF00)){
      ++Cycles;
    }
    ProgramCounter = AbsoluteAddress;
  }
  return 0;
}

uint8_t CPU::CLC()
{
  SetFlag(Carry, false);
  return 0;
}

uint8_t CPU::CLD()
{
  SetFlag(DecimalMode, false);
  return 0;
}

uint8_t CPU::CLI()
{
  SetFlag(IRQ, false);
  return 0;
}

uint8_t CPU::CLV()
{
  SetFlag(OverFlow, false);
  return 0;
}

/// Compare with Accumulator
uint8_t CPU::CMP()
{
  FetchData();
  uint16_t temp = static_cast<uint16_t>(Accumulator) - static_cast<uint16_t>(Fetched);
  SetFlag(Carry, (Accumulator >= FetchData()));
  SetFlag(Zero, (temp & 0x00FF) == 0x0000);
  SetFlag(Negative, temp & 0x0080);
  return 0;
}

/// Compare with X register
uint8_t CPU::CPX()
{
  FetchData();
  uint16_t temp = static_cast<uint16_t>(XRegister) - static_cast<uint16_t>(Fetched);
  SetFlag(Carry, (XRegister >= FetchData()));
  SetFlag(Zero, (temp & 0x00FF) == 0x0000);
  SetFlag(Negative, temp & 0x0080);
  return 0;
}

uint8_t CPU::CPY()
{
  FetchData();
  uint16_t temp = static_cast<uint16_t>(YRegister) - static_cast<uint16_t>(Fetched);
  SetFlag(Carry, (YRegister >= FetchData()));
  SetFlag(Zero, (temp & 0x00FF) == 0x0000);
  SetFlag(Negative, temp & 0x0080);
  return 0;
}

/// Decrement Memory by one
uint8_t CPU::DEC()
{
  FetchData();
  uint16_t temp = Fetched-1;
  Write(AbsoluteAddress, temp & 0x00FF);
  SetFlag(Negative, (temp & 0x0080));
  SetFlag(Zero, ((temp & 0x00FF) == 0x0000));
  return 0;
}

/// Decrement X Register
uint8_t CPU::DEX()
{
  XRegister--;
  SetFlag(Zero, XRegister == 0x00);
  SetFlag(Negative, XRegister & 0x80);
  return 0;
}

/// Devcrement YRegister
uint8_t CPU::DEY()
{
  YRegister--;
  SetFlag(Zero, YRegister == 0x00);
  SetFlag(Negative, YRegister & 0x80);
  return 0;

}

/// Bitwise Exclusive OR (^);
uint8_t CPU::EOR()
{
  FetchData();
  Accumulator = Accumulator ^ Fetched;
  SetFlag(Zero, Accumulator == 0x00);
  SetFlag(Negative, Accumulator & 0x80);
  return 0;
}

/// Increment
uint8_t CPU::INC()
{
  FetchData();
  uint16_t temp = Fetched+1;
  Write(AbsoluteAddress, temp & 0x00FF);
  SetFlag(Negative, (temp & 0x0080));
  SetFlag(Zero, ((temp & 0x00FF) == 0x0000));
  return 0;
}

uint8_t CPU::INX()
{
  XRegister++;
  SetFlag(Zero, XRegister == 0x00);
  SetFlag(Negative, XRegister & 0x80);
  return 0;
}

uint8_t CPU::INY()
{
  YRegister--;
  SetFlag(Zero, YRegister == 0x00);
  SetFlag(Negative, YRegister & 0x80);
  return 0;
}

uint8_t CPU::JMP()
{
  ProgramCounter = AbsoluteAddress;
  return 0;
}

/// Jump to Sub-Routine
uint8_t CPU::JSR()
{
  /// 0x0100 is the Stack offset
  ProgramCounter--;
  Write(0x0100 + StackPointer, (ProgramCounter >> 8) & 0x00FF);
  StackPointer--;
  Write(0x0100 + StackPointer, ProgramCounter & 0x00FF);
  StackPointer--;
  ProgramCounter = AbsoluteAddress;
  return 0;
}

/// Load the Accumulator
uint8_t CPU::LDA()
{
  FetchData();
  Accumulator = Fetched;
  SetFlag(Zero, Accumulator == 0x00);
  SetFlag(Negative, Accumulator & 0x80);
  return 1;
}

uint8_t CPU::LDX()
{
  FetchData();
  XRegister = Fetched;
  SetFlag(Zero, XRegister == 0x00);
  SetFlag(Negative, XRegister & 0x80);
  return 1;
}

uint8_t CPU::LDY()
{
  FetchData();
  YRegister = Fetched;
  SetFlag(Zero, YRegister == 0x00);
  SetFlag(Negative, YRegister & 0x80);
  return 1;

}

/// Shift one bit to the right
uint8_t CPU::LSR()
{
  FetchData();
  SetFlag(Carry, Fetched & (1 << 0));
  uint16_t temp = Fetched >> 1;
  SetFlag(Zero, (temp & 0x00FF) == 0x0000);
  SetFlag(Negative, (temp & 0x0080) );
  if(Lookup[OpCode].AddressMode == &CPU::IMP) {
    Accumulator = temp & 0x00FF;
  } else {
    Write(AbsoluteAddress, temp & 0x00FF);
  }
  return 0;
}

uint8_t CPU::NOP()
{
  switch (OpCode) {
  case 0x1C:
  case 0x3C:
  case 0x5C:
  case 0x7C:
  case 0xDC:
  case 0xFC:
    return 1;
    break;
  }
  return 0;
}

/// Bitwise Logic Or with the accumulator
uint8_t CPU::ORA()
{
  FetchData();
  Accumulator = Accumulator | Fetched;
  SetFlag(Zero, Accumulator == 0x00);
  SetFlag(Negative, (Accumulator & 0x80));
  return 1;
}

/// Push Accumulator to Stack
uint8_t CPU::PHA()
{
  Write(0x0100 + StackPointer, Accumulator);
  StackPointer--;
  return 0;
}

/// Push StatusRegister to Stack
/// Set B flag to 1 before pushing
uint8_t CPU::PHP()
{
  Write(0x0100 + StackPointer, StatusRegister | Break | Unused);
  SetFlag(Break, 0);
  SetFlag(Unused, 0);
  StackPointer--;
  return 0;
}

/// Pop Accumulator off stack
uint8_t CPU::PLA()
{
  StackPointer++;
  Accumulator = Read(0x0100 + StackPointer);
  SetFlag(Zero, Accumulator == 0x00);
  SetFlag(Negative, (Accumulator & 0x80));
  return 0;
}

/// Pop SR off Stacj
uint8_t CPU::PLP()
{
  StackPointer++;
  StatusRegister = Read(0x0100 + StackPointer);
  SetFlag(Unused, 1);
  return 0;
}

/// Rotate one bit left
uint8_t CPU::ROL()
{
  FetchData();
  uint16_t temp = static_cast<uint16_t>(Fetched << 1) | GetFlag(Carry);
  SetFlag(Carry, Fetched & (0xFF00));
  SetFlag(Zero, (temp & 0x00FF) == 0x0000);
  SetFlag(Negative, (temp & 0x0080) );
  if(Lookup[OpCode].AddressMode == &CPU::IMP) {
    Accumulator = temp & 0x00FF;
  } else {
    Write(AbsoluteAddress, temp & 0x00FF);
  }
  return 0;
}

/// Rotate one bit right
uint8_t CPU::ROR()
{
  FetchData();
  uint16_t temp = static_cast<uint16_t>(Fetched >> 1) | static_cast<uint16_t>(GetFlag(Carry) << 7);
  SetFlag(Carry, Fetched & (0x01));
  SetFlag(Zero, (temp & 0x00FF) == 0x0000);
  SetFlag(Negative, (temp & 0x0080) );
  if(Lookup[OpCode].AddressMode == &CPU::IMP) {
    Accumulator = temp & 0x00FF;
  } else {
    Write(AbsoluteAddress, temp & 0x00FF);
  }
  return 0;
}

/// Return from interrupt
/// Pull SR Pull PC
uint8_t CPU::RTI()
{
  StackPointer++;
  StatusRegister = Read(0x0100 + StackPointer);
  StatusRegister &= ~Break;
  StatusRegister &= ~Unused;

  StackPointer++;
  ProgramCounter = static_cast<uint16_t>(Read(0x0100 + StackPointer));
  StackPointer++;
  ProgramCounter |= static_cast<uint16_t>(Read(0x0100 + StackPointer)) << 8;
  return 0;
}

/// Return from SubRoutine
uint8_t CPU::RTS()
{
  StackPointer++;
  ProgramCounter = static_cast<uint16_t>(Read(0x0100 + StackPointer));
  StackPointer++;
  ProgramCounter |= static_cast<uint16_t>(Read(0x0100 + StackPointer)) << 8;
  StackPointer++;
  return 0;
}

/// Similar to the add with carry case:
/// Function:    A = A - M - (1 - C)
/// Flags Out:   C, V, N, Z
///
/// Explanation:
/// Given the explanation for ADC above, we can reorganise our data
/// to use the same computation for addition, for subtraction by multiplying
/// the data by -1, i.e. make it negative
///
/// A = A - M - (1 - C)  ->  A = A + -1 * (M - (1 - C))  ->  A = A + (-M + 1 + C)
///
/// To make a signed positive number negative, we can invert the bits and add 1
/// (OK, I lied, a little bit of 1 and 2s complement :P)
///
///  5 = 00000101
/// -5 = 11111010 + 00000001 = 11111011 (or 251 in our 0 to 255 range)
///
/// The range is actually unimportant, because if I take the value 15, and add 251
/// to it, given we wrap around at 256, the result is 10, so it has effectively
/// subtracted 5, which was the original intention. (15 + 251) % 256 = 10
///
/// Note that the equation above used (1-C), but this got converted to + 1 + C.
/// This means we already have the +1, so all we need to do is invert the bits
/// of M, the data(!) therfore we can simply add, exactly the same way we did
/// before.
uint8_t CPU::SBC()
{
  FetchData();
  /// invert the bottom 8 bits of the fetched data, with xor
  uint16_t Value = ((uint16_t) Fetched)^0x00FF;
  /// from here is just addition,
  uint16_t temp = Value + (uint16_t) Accumulator + (uint16_t) GetFlag(Carry);
  SetFlag(Carry,temp & 0xFF00);
  SetFlag(Zero, (temp & 0xFF00) == 0);
  SetFlag(OverFlow, (temp ^ (uint16_t) Accumulator) & (temp ^ Value) & 0x0080);
  SetFlag(Negative, temp & 0x0080);
  Accumulator = temp & 0x00FF;
  return 1;
}

uint8_t CPU::SEC()
{
  SetFlag(Carry, true);
  return 0;
}

uint8_t CPU::SED()
{
  SetFlag(DecimalMode, true);
  return 0;
}

uint8_t CPU::SEI()
{
  SetFlag(IRQ, true);
  return 0;
}

/// Store Accumulator at Address
uint8_t CPU::STA()
{
  Write(AbsoluteAddress, Accumulator);
  return 0;
}

uint8_t CPU::STX()
{
  Write(AbsoluteAddress, XRegister);
  return 0;
}

uint8_t CPU::STY()
{
  Write(AbsoluteAddress, YRegister);
  return 0;
}

/// Transfer Accumulator to XReg
uint8_t CPU::TAX()
{
  XRegister = Accumulator;
  SetFlag(Zero, XRegister == 0x00);
  SetFlag(Negative, (XRegister & 0x80));
  return 0;
}

uint8_t CPU::TAY()
{
  YRegister = Accumulator;
  SetFlag(Zero, YRegister == 0x00);
  SetFlag(Negative, (YRegister & 0x80));
  return 0;
}

/// Transfer StackPointer to XReg
uint8_t CPU::TSX()
{
  XRegister = StackPointer;
  SetFlag(Zero, XRegister == 0x00);
  SetFlag(Negative, (XRegister & 0x80));
  return 0;
}

/// Transfer X Register to Accumulator
uint8_t CPU::TXA()
{
  Accumulator =  XRegister;
  SetFlag(Zero, Accumulator == 0x00);
  SetFlag(Negative, (Accumulator & 0x80));
  return 0;
}

uint8_t CPU::TXS()
{
  StackPointer = XRegister;
  return 0;
}

uint8_t CPU::TYA()
{
  YRegister = Accumulator;
  SetFlag(Zero, YRegister == 0x00);
  SetFlag(Negative, (YRegister & 0x80));
  return 0;
}

uint8_t CPU::XXX()
{
 return 0;
}

void CPU::Clock()
{
  if (Cycles == 0){
    OpCode = Read(ProgramCounter);
    SetFlag(Unused, true);
    ProgramCounter++;
    Cycles = Lookup[OpCode].Cycles;
    uint8_t AdditionalCycles1 = (this->*Lookup[OpCode].AddressMode)();
    uint8_t AdditionalCycles2 = (this->*Lookup[OpCode].OperationCode)();
    Cycles += (AdditionalCycles1 & AdditionalCycles2);
    SetFlag(Unused,true);
  }
  ClockCount++;
  Cycles--;
}

void CPU::Reset()
{
  /// Read address from 0xFFFC and 0xFFFD
  AbsoluteAddress = 0xFFFC;
  uint16_t ByteLow = Read(AbsoluteAddress + 0);
  uint16_t ByteHi = Read(AbsoluteAddress + 1);

  /// set the program counter
  ProgramCounter = (ByteHi << 8) | ByteLow;

  /// Reset registers
  Accumulator    = 0x00;
  XRegister      = 0x00;
  YRegister      = 0x00;
  StackPointer   = 0xFD;
  StatusRegister = 0x00 | Unused;

  /// Clear Variables
  RelativeAddress = 0x00;
  AbsoluteAddress = 0x00;
  Fetched         = 0x00;

  Cycles = 8;
}

void CPU::InterruptRequest()
{

}

void CPU::NonMaskableInterruptRequest()
{

}

uint8_t CPU::GetFlag(Flags6502 f)
{
  return ((StatusRegister & f) > 0) ? 1 : 0;
}

void CPU::SetFlag(Flags6502 f,bool v)
{
  if(v) StatusRegister |= f;
  else  StatusRegister &= ~f;
}

uint8_t CPU::Read(uint16_t Address)
{
  /// pass the call to the bus, and read data from the given
  /// address
  return _Bus->Read(Address);
}

void CPU::Write(uint16_t Address, uint8_t Data)
{
  _Bus->Write(Address, Data);
}

uint8_t CPU::FetchData()
{
  /// Lookup mode implied doesn't need to read from address
  if(!( Lookup[OpCode].AddressMode == &CPU::IMP)){
    Fetched = Read(AbsoluteAddress);
  }
  return Fetched;
}

void CPU::ShowStatus()
{
  std::cout << std::hex << "  Accumulator    :    " << static_cast<long unsigned int>(Accumulator)    << "\n"
	    << std::hex << "  XRegister      :    " << static_cast<long unsigned int>(XRegister)      << "\n"
	    << std::hex << "  YRegister      :    " << static_cast<long unsigned int>(YRegister)      << "\n"
	    << std::hex << "  StackPointer   :    " << static_cast<long unsigned int>(StackPointer)   << "\n"
	    << std::hex << "  ProgramCounter :    " << static_cast<long unsigned int>(ProgramCounter) << "\n";
}

// This is the disassembly function. Its workings are not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.

std::map<uint16_t, std::string> CPU::Disassemble(uint16_t nStart, uint16_t nStop)
{
  uint32_t addr = nStart;
  uint8_t value = 0x00, lo = 0x00, hi = 0x00;
  std::map<uint16_t, std::string> mapLines;
  uint16_t line_addr = 0;

  // A convenient utility to convert variables into
  // hex strings because "modern C++"'s method with
  // streams is atrocious
  auto hex = [](uint32_t n, uint8_t d)
  {
    std::string s(d, '0');
    for (int i = d - 1; i >= 0; i--, n >>= 4)
      s[i] = "0123456789ABCDEF"[n & 0xF];
    return s;
  };

  // Starting at the specified address we read an instruction
  // byte, which in turn yields information from the lookup table
  // as to how many additional bytes we need to read and what the
  // addressing mode is. I need this info to assemble human readable
  // syntax, which is different depending upon the addressing mode

  // As the instruction is decoded, a std::string is assembled
  // with the readable output
  while (addr <= (uint32_t)nStop)
    {
      line_addr = addr;

      // Prefix line with instruction address
      std::string sInst = "$" + hex(addr, 4) + ": ";

      // Read instruction, and get its readable name
      uint8_t opcode = _Bus->Read(addr, true); addr++;
      sInst += Lookup[opcode].name + " ";

      // Get oprands from desired locations, and form the
      // instruction based upon its addressing mode. These
      // routines mimmick the actual fetch routine of the
      // 6502 in order to get accurate data as part of the
      // instruction
      if (Lookup[opcode].AddressMode == &CPU::IMP)
	{
	  sInst += " {IMP}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::IMM)
	{
	  value = _Bus->Read(addr, true); addr++;
	  sInst += "#$" + hex(value, 2) + " {IMM}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::ZP0)
	{
	  lo = _Bus->Read(addr, true); addr++;
	  hi = 0x00;
	  sInst += "$" + hex(lo, 2) + " {ZP0}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::ZPX)
	{
	  lo = _Bus->Read(addr, true); addr++;
	  hi = 0x00;
	  sInst += "$" + hex(lo, 2) + ", X {ZPX}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::ZPY)
	{
	  lo = _Bus->Read(addr, true); addr++;
	  hi = 0x00;
	  sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::IZX)
	{
	  lo = _Bus->Read(addr, true); addr++;
	  hi = 0x00;
	  sInst += "($" + hex(lo, 2) + ", X) {IZX}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::IZY)
	{
	  lo = _Bus->Read(addr, true); addr++;
	  hi = 0x00;
	  sInst += "($" + hex(lo, 2) + "), Y {IZY}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::ABS)
	{
	  lo = _Bus->Read(addr, true); addr++;
	  hi = _Bus->Read(addr, true); addr++;
	  sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + " {ABS}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::ABX)
	{
	  lo = _Bus->Read(addr, true); addr++;
	  hi = _Bus->Read(addr, true); addr++;
	  sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", X {ABX}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::ABY)
	{
	  lo = _Bus->Read(addr, true); addr++;
	  hi = _Bus->Read(addr, true); addr++;
	  sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", Y {ABY}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::IND)
	{
	  lo = _Bus->Read(addr, true); addr++;
	  hi = _Bus->Read(addr, true); addr++;
	  sInst += "($" + hex((uint16_t)(hi << 8) | lo, 4) + ") {IND}";
	}
      else if (Lookup[opcode].AddressMode == &CPU::REL)
	{
	  value = _Bus->Read(addr, true); addr++;
	  sInst += "$" + hex(value, 2) + " [$" + hex(addr + value, 4) + "] {REL}";
	}

      // Add the formed string to a std::map, using the instruction's
      // address as the key. This makes it convenient to look for later
      // as the instructions are variable in length, so a straight up
      // incremental index is not sufficient.
      mapLines[line_addr] = sInst;
    }

  return mapLines;
}
