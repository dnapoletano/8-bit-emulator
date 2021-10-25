#ifndef EIGHTBIT_EMULATOR_BUS_HPP
#define EIGHTBIT_EMULATOR_BUS_HPP

#include <array>
#include <cstdint>
#include <memory>
#include <vector>
class CPU;

class Bus
{
private:
public:
  Bus();
  ~Bus();

  /// R/W functions
  uint8_t Read(uint16_t Address, bool flag = true);
  void Write(uint16_t Address, uint8_t Data);
  std::unique_ptr<CPU> _CPU;
  /// 64 kB of ram to read from and write to
  std::array<uint8_t, 64 * 1024> _RAM;

};

#endif
