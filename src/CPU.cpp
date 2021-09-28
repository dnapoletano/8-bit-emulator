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

uint8_t CPU::IMP() 
{
  
}

uint8_t CPU::REL() 
{
  
}

uint8_t CPU::IMM() 
{
  
}

uint8_t CPU::ABY() 
{
  
}

uint8_t CPU::ABS() 
{
  
}

uint8_t CPU::ABX() 
{
  
}

uint8_t CPU::ZP0() 
{
  
}

uint8_t CPU::IZX() 
{
  
}

uint8_t CPU::ZPX() 
{
  
}

uint8_t CPU::IZY() 
{
  
}

uint8_t CPU::ZPY() 
{
  
}

uint8_t CPU::IND() 
{
  
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
