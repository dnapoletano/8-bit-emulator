#include "CPU.hpp"

CPU::CPU()
: Accumulator{0x00}, XRegister{0x00}, YRegister{0x00},
  StackPointer{0x00}, ProgramCounter{0x0000}, StatusRegister{0x00},
  Fetched{0x00}, AbsoluteAddress{0x0000}, RelativeAddress{0x0000},
  OpCode{0x00}, Cycles{0}, ClockCount{0}
{

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


uint8_t CPU::ADC()
{

}

uint8_t CPU::AND()
{

}

uint8_t CPU::ASL()
{

}

uint8_t CPU::BCC()
{

}

uint8_t CPU::BCS()
{

}

uint8_t CPU::BEQ()
{

}

uint8_t CPU::BIT()
{

}

uint8_t CPU::BMI()
{

}

uint8_t CPU::BNE()
{

}

uint8_t CPU::BPL()
{

}

uint8_t CPU::BRK()
{

}

uint8_t CPU::BVC()
{

}

uint8_t CPU::BVS()
{

}

uint8_t CPU::CLC()
{

}

uint8_t CPU::CLD()
{

}

uint8_t CPU::CLI()
{

}

uint8_t CPU::CLV()
{

}

uint8_t CPU::CMP()
{

}

uint8_t CPU::CPX()
{

}

uint8_t CPU::CPY()
{

}

uint8_t CPU::DEC()
{

}

uint8_t CPU::DEX()
{

}

uint8_t CPU::DEY()
{

}

uint8_t CPU::EOR()
{

}

uint8_t CPU::INC()
{

}

uint8_t CPU::INX()
{

}

uint8_t CPU::INY()
{

}

uint8_t CPU::JMP()
{

}

uint8_t CPU::JSR()
{

}

uint8_t CPU::LDA()
{

}

uint8_t CPU::LDX()
{

}

uint8_t CPU::LDY()
{

}

uint8_t CPU::LSR()
{

}

uint8_t CPU::NOP()
{

}

uint8_t CPU::ORA()
{

}

uint8_t CPU::PHA()
{

}

uint8_t CPU::PHP()
{

}

uint8_t CPU::PLA()
{

}

uint8_t CPU::PLP()
{

}

uint8_t CPU::ROL()
{

}

uint8_t CPU::ROR()
{

}

uint8_t CPU::RTI()
{

}

uint8_t CPU::RTS()
{

}

uint8_t CPU::SBC()
{

}

uint8_t CPU::SEC()
{

}

uint8_t CPU::SED()
{

}

uint8_t CPU::SEI()
{

}

uint8_t CPU::STA()
{

}

uint8_t CPU::STX()
{

}

uint8_t CPU::STY()
{

}

uint8_t CPU::TAX()
{

}

uint8_t CPU::TAY()
{

}

uint8_t CPU::TSX()
{

}

uint8_t CPU::TXA()
{

}

uint8_t CPU::TXS()
{

}

uint8_t CPU::TYA()
{

}

void CPU::Clock()
{

}

void CPU::Reset()
{

}

void CPU::InterruptRequest()
{

}

void CPU::NonMaskableInterruptRequest()
{

}

uint8_t CPU::Read(uint16_t Address)
{
  return 0;
}

void CPU::Write(uint16_t Address, uint8_t Data)
{

}

uint8_t CPU::FetchData()
{
  /// Lookup mode implied doesn't need to read from address
  if(!( Lookup[OpCode].AddressMode == &CPU::IMP)){
    Fetched = Read(AbsoluteAddress);
  }
  return Fetched;
}
