#include "Bus.hpp"
#include "CPU.hpp"

Bus::Bus()
  : _CPU{new CPU{}}
{
  _CPU->ConnectBus(this);
  for (auto &r: _RAM) r = 0x00;
}

Bus::~Bus() { }

uint8_t Bus::Read(uint16_t Address, bool flag)
{
  if(Address >= 0x0000 and Address <= 0xFFFF){
    return _RAM[Address];
  }
  return 0x00;
}

void Bus::Write(uint16_t Address, uint8_t Data)
{
  /// check that address is in valid range (0xFFFF = 65535 = 64 * 1024 -1)
  if(Address >= 0x0000 and Address <= 0xFFFF){
    _RAM[Address] = Data;
  }
}
