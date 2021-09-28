#ifndef EIGHTBIT_EMULATOR_CPU_HPP
#define EIGHTBIT_EMULATOR_CPU_HPP

#include <cstdint>
/** 6502 model:
 *  This CPU has 16-bit address and 8-bit data.
 *  Thus, the full addressable memory range is $0000 - $FFFF (64kB).
 *  It needs a clock to be able to work, and a r/w flag to determine
 *  whether its status is reading or writing.
 *  We connect the CPU to the bus, such that other devices can
 *  respond to addresses put on the BUS (RAM essentially).
 *  For the time being we assume that only RAM is connected to the
 *  BUS, with the full 64kB of accessible memory.
 *  
 * Inside the 6502 we have:
 * - A-Accumulator (8-bit)
 * - X-Register (8-bit)
 * - Y-Register (8-bit)
 * - Stack Pointer (8-bit)
 * - Program Counter (16-bit), stores the address of the next operation
 * - Status Register (8-bit of single flags):
 *    - Bit7: Negative Flag,                   1 = Negative
 *    - Bit6: Overflow,                        1 = True
 *    - Bit5: Unset,
 *    - Bit4: Break,                           1 = Break
 *    - Bit3: Decimal Mode,                    1 = True
 *    - Bit2: Interrupt Request Disable,       1 = Disable
 *    - Bit1: Zero,                            1 = Result equaled Zero
 *    - Bit0: Carry Flag                       1 = True
 *    
 */

class CPU
{
  private:
    uint8_t Accumulator;
    uint8_t XRegister;
    uint8_t YRegister;
    uint8_t StackPointer;
    uint16_t ProgramCounter;
    uint8_t StatusRegister;
    enum Flags6502{
      Carry       = (1 << 0),
      Zero        = (1 << 1),
      IRQ         = (1 << 2),
      DecimalMode = (1 << 3),
      Break       = (1 << 4),
      Unused      = (1 << 5),
      OverFlow    = (1 << 6),
      Negative    = (1 << 7),
    };

    uint8_t FetchData();
    uint8_t Fetched;
    uint16_t AbsoluteAddress;
    uint16_t RelativeAddress;
    uint8_t OpCode;
    uint8_t Cycles;
    uint32_t ClockCount;

    void Clock();
    void Reset();
    void InterruptRequest();
    void NonMaskableInterruptRequest();

  private: /// Addressing modes
    uint8_t IMP();  uint8_t REL();
    uint8_t IMM();  uint8_t ABY();
    uint8_t ABS();  uint8_t ABX();
    uint8_t ZP0();  uint8_t IZX();
    uint8_t ZPX();  uint8_t IZY();
    uint8_t ZPY();  uint8_t IND();

  private: /// Op-Codes
	  uint8_t ADC();	uint8_t AND();	uint8_t ASL();	uint8_t BCC();
	  uint8_t BCS();	uint8_t BEQ();	uint8_t BIT();	uint8_t BMI();
	  uint8_t BNE();	uint8_t BPL();	uint8_t BRK();	uint8_t BVC();
	  uint8_t BVS();	uint8_t CLC();	uint8_t CLD();	uint8_t CLI();
	  uint8_t CLV();	uint8_t CMP();	uint8_t CPX();	uint8_t CPY();
	  uint8_t DEC();	uint8_t DEX();	uint8_t DEY();	uint8_t EOR();
	  uint8_t INC();	uint8_t INX();	uint8_t INY();	uint8_t JMP();
	  uint8_t JSR();	uint8_t LDA();	uint8_t LDX();	uint8_t LDY();
	  uint8_t LSR();	uint8_t NOP();	uint8_t ORA();	uint8_t PHA();
	  uint8_t PHP();	uint8_t PLA();	uint8_t PLP();	uint8_t ROL();
	  uint8_t ROR();	uint8_t RTI();	uint8_t RTS();	uint8_t SBC();
	  uint8_t SEC();	uint8_t SED();	uint8_t SEI();	uint8_t STA();
	  uint8_t STX();	uint8_t STY();	uint8_t TAX();	uint8_t TAY();
	  uint8_t TSX();	uint8_t TXA();	uint8_t TXS();	uint8_t TYA();

	  /// "unofficial" opcodes
	  uint8_t XXX();

  public:
    CPU();
    ~CPU();
};

#endif