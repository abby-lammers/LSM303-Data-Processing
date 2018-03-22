/**************************************
   Fast code for testing tilt and yaw calculations
   SD and Serial output
   RTC alarm disabled

   Original code: Ed Mallon (https://github.com/EKMallon)
   Edited by: Abby Lammers
   view on Github at https://github.com/thisislammers/LSM303-Data-Processing

 * *************************************/


// this is fast code for calibration only - temperature reading has been removed from the code!
// as this code has no sleep delay or RTC alarms (sleepNwait4RTC is commented out) so at one reading per second there is
// not enough time to get data from the DS18b20 temp sensor

//  if you want to include a header file which isn't in the library path, you use double quotes, instead of angle brackets:
#include <Wire.h>       // I2C lib needs 128 byte Serial buffer
#include <SPI.h>        // not used here, but needed to prevent a RTClib compile error....grrr!
#include <RTClib.h>     // Date, Time and Alarm functions by https://github.com/MrAlvin/RTClib for DS1307, DS3231 chips
#include <LowPower.h>   // https://github.com/rocketscream/Low-Power
#include <avr/sleep.h>
//#include <avr/wdt.h>  // re-enable this if you switch to the older WDT sleeping routines
#include <PString.h>    // from  http://arduiniana.org/
#include <SdFat.h>      // from https://github.com/greiman/SdFat needs a 512 byte ram buffer
//SdFat SD;

// Wait for serial monitor to run code (prevents excess header info from being written)
#define WAIT_TO_START 1;

SdFat sd; /*Create the objects to talk to the SD card*/
SdFile file;
const byte chipSelect = 10; //SPI chip select pin for the SD card

#define ECHO_TO_SERIAL   // for debugging  this takes about the same memory as doing the first order pressurecalculations!

//FUNDAMENTAL OPERATING PARAMETERS:
//---------------------------------
#define SampleIntervalMinutes 1   // power-down time in minutes before RTC interrupt triggers the next cycle
#define SampleIntSeconds 0 // this is ONLY used for DEBUGGING! otherwise SET to 0! SampleIntervalMinutes must be set to zero for Second interval alarms to occur!
// WARNING: if any of your sensors take more time to read & process than the amount of time you set for the sampleinterval the logger will freeze up.
// this is fast code for calibration only - temperature reading has been removed from the code!
// as this code has no sleep delay or RTC alarms (sleepNwait4RTC is cmented out) so at one reading per second there is
// not enough time to get data from the DS18b20 temp sensor

char unitNotes[] = "Codebuild:Utility_LSM303_FAST4calibration_noDS18_Tilt"; // name&version# of this script
char loggerNumber[] = "#xxx Cap output test -LM303,Fast4Calibration";

#define SamplesPerCycle 60  // AT24C32 eeprom on RTC module can only hold 128 "page" // 1024 "pages" can be stored on AT24C256 
// so Maximum value of Samples per cycle = # of 32byte pages eeprom can hold / PagesBuffered2Eeprom
#define EEPROM_ADDRESS 0x50  // RTC eeprom at 0x57, AT24C256 at 0x50 if installed
uint8_t PagesBuffered2Eeprom = 3; // used by flushEepromBuffer function! For the 1st buffer dump in setup, this must match the number of "pages" you used to store each record, auto adjusts afterwards
unsigned int fileInterval = 1440; // #of log records before new logfile is made usually 1440 (about 2 weeks worth of data) Must be less than 65,535 counts per file
//If fileInterval is 15min, then 2880 = 96samples/day * 30days/month = 30 day intervals
#define RTCPOWER_PIN 7       // When the arduino is awake,powering the rtc from this pin (70uA), when arduino sleeps pin set low & rtc runs on battery at <3uA
#define vRegulatedMCU 1  //Pro Mini needs to use a voltage divider on analog pin 0


//variables needed if you
//#include <Comp6DOF_n0m1.h>
// variables for the Calculation of compass headings from Comp6DOF_n0m1

const int K1 = 5701;  //used in the atanInt function
const int K2 = -1645;
const int K3 = 446;
const unsigned int MINDELTADIV = 1;              /* final step size for divInt */

#define SINE_INDEX_WIDTH 4
#define SINE_INTERP_WIDTH 8

#if (SINE_INDEX_WIDTH + SINE_INTERP_WIDTH > 12)
# error Invalid sine widths
#endif

#define SINE_INDEX_OFFSET (12 - SINE_INDEX_WIDTH)
#define SINE_INTERP_OFFSET (SINE_INDEX_OFFSET - SINE_INTERP_WIDTH)
#define QUADRANT_HIGH_MASK (1 << 13)
#define QUADRANT_LOW_MASK (1 << 12)
//#define MAX(a, b) ((a) < (b) ? (b) : (a))

#if SINE_INTERP_OFFSET > 0
# define SINE_ROUNDING (1 << (SINE_INTERP_OFFSET-1))
#else
# define SINE_ROUNDING (0)
#endif

#define BITS(_VALUE_, _WIDTH_, _BIT_) (((_VALUE_) >> (_BIT_)) & ((1 << (_WIDTH_)) - 1))
#define SINE_TABLE_SIZE (1 << SINE_INDEX_WIDTH)
// Table of the first quadrant values.  Size is + 1 to store the first value of
// the second quadrant, hence we're storing 0 <= degrees <= 90.
static const int16_t sinLUT[SINE_TABLE_SIZE + 1] = {
  0,  3211,  6392,  9511, 12539, 15446, 18204, 20787,
  23169, 25329, 27244, 28897, 30272, 31356, 32137, 32609,
  32767
};

#define TRIGINT_ANGLES_PER_CYCLE 0x4000

int roll_;
int pitch_;
int yaw_;

int lpRoll_;
int lpPitch_;
int lpYaw_;

int xMagAxisComp_;
int yMagAxisComp_;
int zMagAxisComp_;


//*****LSM303 variables**********************************************************
// LM303 Compass  //see readLSM303

//#if defined(HMC5883_ADDRESS) || defined(LSM303_ADDRESS_MAG)
int Xm_raw, Ym_raw, Zm_raw;
float Xm_off, Ym_off, Zm_off;
float Xm_cal, Ym_cal, Zm_cal;


//#endif

//#if defined(LSM303_ADDRESS_ACCEL)
// LM303 accelerometer:
int Xa_raw, Ya_raw, Za_raw;  // different from the  int rawACCx [filterSamples]; used by older accelerometers because I am not smoothing!
float Xa_off, Ya_off, Za_off;
float Xa_cal, Ya_cal, Za_cal;

/*
  float Xa_bias=-8971.47;
  float Ya_bias=-1514.64;
  float Za_bias=-1347.74;
  float Xa_x=1.075141; float Xa_y=0.013438; float Xm_a=0.015174;
  float Ya_x=0.013438; float Ya_y=1.044311; float Ym_a=-0.007061;
  float Za_x=0.015174; float Za_y=-0.007061; float Zm_a=0.999432;
*/
//#endif

int Gproj_2_XYplane;
int Angle_of_Gproj;
int tiltAngle;
int LSM303_TEMP_Raw = 0;

//int lsm303MagX;  //only used in the nom1 calculations
//int lsm303MagY;
//int lsm303MagZ;



// What sensors are connected?
//----------------------------
//#define TS_DS18B20 INSTALLED
#define LSM303_ADDRESS_ACCEL (0x32 >> 1)  // 0011001x The Arduino takes care of the last R/W bit for us depending on what function we’re using 
#define LSM303_ADDRESS_MAG   (0x3C >> 1)  // 0011110x  so as long as you’re using the standard Arduino Wire library we don’t have to worry about this bit.
#define LSM303_ID 0b11010100
//note last bit of lm303 address is read/write bit

unsigned int countLogs = 0;               // # records have been written to each file, when countLogs=fileInterval make a new log file
char FileName[] = "LOG00000.CSV";         //the first file name
char EPRbuffFileName[] = "EPBuffer.CSV";  //file to dump eeprom contents to on startup.
char TempName[] = "LOG00000.CSV";         //used to swap file names before entering eeprombufferdump function

int PSvolts = 9999; //the supply voltage (via 1.1 internal band gap OR analog read)
int Vcc2 = 9999; //the post SD card writing supply voltage, 9999 until first card write cycle
int CoinCellV = 9999; // note: with echo to serial enabled readCoinCell(); is triggered at each read, otherwise it occurs once per day
//int Vdelta;   //change in supply voltage after each SD write cycle...to track battery conditioin

#if defined(vRegulatedMCU)
const float referenceVolts = 3.35;  // although some of the vregs on the clones go as high as 3.6v! Should measure this for each unit!
const float resistorFactor = 511;   // = 1023.0 * (R2/(R1 + R2));  if R1 and R2 are the same - max measurable input voltage of 6.6v
int CutoffVoltage = 3400;  //most vregs require 3.35v minimum input
#else
int CutoffVoltage = 2900;  //lowest permitable voltage on the unregulated TinyDuinos
#endif
int BlueVWarning = CutoffVoltage + 300; //READSENSOR_PIN's led pip changes from Green -> Blue -> Red based on these numbers
int RedVWarning = CutoffVoltage + 150; //to warn you when the batteries are running low

// 3 color indicator LED pin connections- if there is only one led on your system set all these defines to the same pin number
#define RED_PIN 4
#define GREEN_PIN 5
#define BLUE_PIN 6
byte READSENSOR_PIN = GREEN_PIN;

//Global variables
//******************
byte bytebuffer1 = 0;
byte bytebuffer2 = 0;
int tempInt = 0;
float tempFloat = 0;
bool tempBool;

//AT24C32 I2C eeprom
//******************
//#define EEPROM_ADDRESS 0x50
// I2C Buss address of AT24C32 EEPROM on RTC board with pins pulled high is 0x57
// The AT25C32 (0x57) is internally organized into (32,768 bits)=4096 bytes - beyond 4096 characters it rewrites over top of the data
// (128 x 32byte writes fill entire 4096 byte block) so MAX 64 without compass (2 pgwrites/cycle) but max 42 if compass is installed! (3 pgwrites/cycle)
// the separate AT24C256 has its address pins pulled low, so it is on the bus at 0x50
#define EEPromPageSize 32                 // 32 bytes is page size for the AT24C32
unsigned int CurrentPageStartAddress = 0; // set to zero at the start of each cycle
char EEPROMBuffer[28]; // this buffer contains a string of ascii because I am using the pstring function to load it
uint8_t BytesWrittentoSD = 0;
char DayEventBuffer[28]; //this lets me store 28 charaters of once per day data when the RTC hour rolls over

//DS3231 RTC
//**********
#define DS3231_ADDRESS 0x68                 //=104 dec
#define DS3231_STATUS_REG 0x0f
#define DS3231_CTRL_REG 0x0e
#define Bit0_MASK         B00000001        //Bit 0=Alarm 1 Flag (A1F)
#define Bit1_MASK         B00000010        //Bit 1 = Alarm 2 Flag (A2F)
#define Bit2_MASK         B00000100
#define Bit3_MASK         B00001000        //Bit 3: Enable/disable 32kHz Output (EN32kHz) - has no effect on sleep current
#define Bit4_MASK         B00010000        //Bit 4: Bits 4&5 of status reg adjust Time Between Temperature Updates  see http://www.maximintegrated.com/en/app-notes/index.mvp/id/3644
#define Bit5_MASK         B00100000        //Bit 5:
// SEE http://www.gammon.com.au/forum/?id=11497 for an example powering ds1307 from pin
RTC_DS3231 RTC;                            //DS3231 will function with a VCC ranging from 2.3V to 5.5V
byte Alarmhour = 1;
byte Alarmminute = 1;
byte Alarmday = 1;                         //only used for sub second alarms
byte Alarmsecond = 1;                      //only used for sub second alarms
byte INTERRUPT_PIN = 2;                    // RTC SQW is soldered to this pin on the arduino
volatile boolean clockInterrupt = false;
char CycleTimeStamp[ ] = "0000/00/00,00:00"; //16 characters without seconds!
//byte Startday;

// Variables used by the DIGITAL SMOOTHING routine  // Warning: if you take allot of samples you eat up memory!
// ******************************************************************************

#if defined(filterSamples) // Accelerometers and some TMP102 configs use this
int rawACCx [filterSamples];   // raw sensor values for x
int rawACCy [filterSamples];   // raw sensor values for y
int rawACCz [filterSamples];   // raw sensor values for z

int smoothACCx = 0;  // smoothed x data
int smoothACCy = 0;  // smoothed y data
int smoothACCz = 0;  // smoothed z data
#endif

// Temperature sensor common variables
// ***********************************
int TEMP_Raw = 0;
float TEMP_degC = 0.0;
uint8_t wholeTemp = 0;
uint8_t fracTemp = 0;

#ifdef TS_DS18B20    //variables for DS18B20 temperature sensor
#include <OneWire.h>    // from  http://www.pjrc.com/teensy/td_libs_OneWire.html only need if DS18B20 is connected!
// Also see Dallas Temperature library by Miles Burton: http://milesburton.com/Dallas_Temperature_Control_Library
const byte DS18B20_PIN = 8; //we have the onewire bus using digital pin 8
OneWire ds(DS18B20_PIN);
byte addr[8];
#endif


/*******************************************************************************************************************************************
 *******************************************************************************************************************************************
 *  *  *  *  *  *  SET UP  *  *  *  *  *  *
 *******************************************************************************************************************************************
  /******************************************************************************************************************************************/

// errors in setup will ALWAYS call error routine that halts the system
void setup () {

#if defined POWERDOWN_PIN
  pinMode(POWERDOWN_PIN, OUTPUT);
  digitalWrite(POWERDOWN_PIN, LOW);// driving this pin high shuts down the system if you have the Pololu power switch connected
#endif

#ifdef RTCPOWER_PIN
  pinMode(RTCPOWER_PIN, HIGH);
  digitalWrite(RTCPOWER_PIN, OUTPUT);// driving this high supplies power to the RTC Vcc pin while arduino is awake
#endif

  pinMode(INTERRUPT_PIN, INPUT);
  digitalWrite(INTERRUPT_PIN, HIGH);//pull up the RTC interrupt pin

#ifdef ADXL345_ISON
  pinMode(adxl345intPin, INPUT);
  digitalWrite(adxl345intPin, HIGH);// ASDXL345 is on pin (3) pull up the interrupt pin
#endif

  pinMode(RED_PIN, OUTPUT);
  digitalWrite(RED_PIN, LOW);     // warns of low voltage error state, flags when system is doing eeprom & SD card writes
  pinMode(GREEN_PIN, OUTPUT);
  digitalWrite(GREEN_PIN, HIGH);  // usually used for sensor readings
  pinMode(BLUE_PIN, OUTPUT);
  digitalWrite(BLUE_PIN, LOW);    // early low voltage warning

  DayEventBuffer[0] = 0; // contents will not get written to the SD card if it stays empty

  Serial.begin(9600);  //do I need this when running on batteries?  could go to 115200 for faster serial coms?
  Wire.begin();
  RTC.begin();

  // check RTC
  //**********
  clearClockTrigger(); //stops RTC from holding the interrupt low if system reset just occured
  RTC.turnOffAlarm(1);
  i2c_writeRegBits(DS3231_ADDRESS, DS3231_STATUS_REG, 0, Bit3_MASK); // disable the 32khz output  pg14-17 of datasheet  //This does not reduce the sleep current
  i2c_writeRegBits(DS3231_ADDRESS, DS3231_STATUS_REG, 1, Bit4_MASK); // see APPLICATION NOTE 3644 - this might only work on the DS3234?
  i2c_writeRegBits(DS3231_ADDRESS, DS3231_STATUS_REG, 1, Bit5_MASK); // setting bits 4&5 to 1, extends the time between RTC temp updates to 512seconds (from default of 64s)
  DateTime now = RTC.now();
  sprintf(CycleTimeStamp, "%04d/%02d/%02d %02d:%02d", now.year(), now.month(), now.day(), now.hour(), now.minute());

  // startup delays if needed
  //*************************
#ifdef ECHO_TO_SERIAL
  serial_boilerplate();
#endif
#ifndef ECHO_TO_SERIAL
  // ten second delay in here just to prevent power stutters from writing multiple headers to the sd card
  delay(5000); // delay here just to prevent power stutters from writing multiple headers to the sd card
  digitalWrite(GREEN_PIN, LOW);
  digitalWrite(BLUE_PIN, HIGH); // may as well check that the LED's are all working while we are waiting
  delay(5000);
  digitalWrite(BLUE_PIN, LOW);
  digitalWrite(RED_PIN, HIGH);
  delay(5000);
  digitalWrite(RED_PIN, LOW);
  digitalWrite(GREEN_PIN, HIGH);
  // ANOTHER ten second delay in here  SO THAT I CAN GET THE HOUSING ASSEMBLED FOR THE CALIBRATION TESTING.
  delay(5000);
  digitalWrite(GREEN_PIN, LOW);
  digitalWrite(BLUE_PIN, HIGH); // may as well check that the LED's are all working while we are waiting
  delay(5000);
  digitalWrite(BLUE_PIN, LOW);
  digitalWrite(RED_PIN, HIGH);
  delay(5000);
  digitalWrite(RED_PIN, LOW);
  digitalWrite(GREEN_PIN, HIGH);
#endif
  PSvolts = readExternalPSvolts(); //the first check of the system voltage to make sure SD card writing is safe
  // if you have >5M ohm voltage divider, first read is usually ~ 25-50mv low, as the stabilization cap is not charged yet - you need the TIME DELAY above or PSvolts will error out
  // In normal operation readExternalPSvolts errors to shut down if the read voltage is below CutoffVoltage, unless ECHO_TO_SERIAL defined (which bumps the reading read up by 5000 so USB tethered runs can do SD card writes)
  // the PSvolts divider will read 0v (& halt with low voltage ERROR) if ECHO_TO_SERIAL is undefined  --AND-- unit is tethered to USB adapter,
  // SO code without ECHO_TO_SERIAL defined can ONLY start running on batteries unless you modify readExternalPSvolts()

  // This check does the mirror image, it prevents me from deploying code that IS running in ECHO_TO_SERIAL DEBUG mode
#if defined(vRegulatedMCU) && defined(ECHO_TO_SERIAL)
  Vcc2 = PSvolts + 5000;
  if (Vcc2 > 13600) {  // only happens if ECHO_to_SERIAL defined --AND-- We have a regulated board --AND-- there is a valid battery voltage on the divider reading the power supply
    error();
  }
#endif

  readCoinCell();  // usually attached to pin A1

#ifdef WAIT_TO_START  // only triggered if WAIT_TO_START is defined at beging of code
  Serial.println(F("Type any character to start"));
  while (!Serial.available());
#endif


  /*********************************************************
  *  *  *  *  *  *  SENSOR initializations  *  *  *  *  *  *
  **********************************************************/

  //Temp sensor(s) init
  //**********************

#ifdef TS_DS18B20
  if ( !ds.search(addr))
  {
    Serial.println(F("ERROR: Did not find the DS18B20 Temp Sensor!")); Serial.flush();
    return;
  }
  else
  {
#ifdef ECHO_TO_SERIAL
    Serial.print(F("DS18B20 found @ ROM addr:"));
    for (byte i = 0; i < 8; i++) {
      Serial.write(' ');
      Serial.print(addr[i], HEX);
    }
    Serial.println(); Serial.flush();
#endif
  }
#endif

  //Accelerometer init
  //**********************

#ifdef LSM303_ADDRESS_ACCEL
  initLSM303(); //starts both accel & compass
#endif

  //Compass init
  //**********************
#ifdef HMC5883_ADDRESS
  initHMC5883();
#endif

#ifdef HTDU21D_ADDRESS
  initHTU21D();
#endif

#if defined(MS580X_I2C_ADDRESS)
  // Initialize the MS5803 sensor. This will report the
  // conversion coefficients to the Serial terminal if present.
  // If you don't want all the coefficients printed out,
  // set sensor.initializeMS_5803(false) otherwise send true

  if (initializeMS_5803(true)) {
    Serial.println(F("MS5803 CRC  OK." ));
  }
  else {
    Serial.println(F("MS5803 CRC FAILED!") );
  }
#endif
  digitalWrite(GREEN_PIN, LOW);


  /*********************************************************
  *  *  *  *  *  *  SD card initialization  *  *  *  *  *  *
  **********************************************************/
  pinMode(chipSelect, OUTPUT);  //make sure that the default chip select pin is set to output, even if you don't use it

#ifdef ECHO_TO_SERIAL
  Serial.print(F("Init SD card..."));
#endif

  // Initialize SdFat or print a detailed error message and halt
  // Use SPI_HALF_SPEED to behave like the native library. // change to SPI_FULL_SPEED for more performance.
  // SPI (Serial Peripheral Interface): http://www.gammon.com.au/spi
  // SD card latencies vary randomly between 41ms to 300ms and block Arduino in the meantime: http://forum.arduino.cc/index.php?topic=256084.0
  if (!sd.begin(chipSelect, SPI_FULL_SPEED)) {
    Serial.println(F("Cound NOT initialize SD Card")); Serial.flush();
    error();
  }

#ifdef ECHO_TO_SERIAL
  Serial.print(F("SD init @:"));
  Serial.println(CycleTimeStamp);
  Serial.print(F("The sample interval is: "));
  Serial.print(SampleIntervalMinutes);
  Serial.print(F(" min."));
#ifdef SampleIntSeconds
  Serial.print(F(" "));
  Serial.print(SampleIntSeconds);
  Serial.print(F(" Sec."));
#endif
  Serial.println();
  Serial.flush();
#endif

  // open the file for write at end like the Native SD library
  // see http://forum.arduino.cc/index.php?topic=49649.0
  // O_CREAT = create the file if it does not exist
  if (!file.open(FileName, O_RDWR | O_CREAT | O_AT_END)) {
    Serial.println(F("1st open LOG.CSV fail"));
    error();
  }

  writeHeaderInfo();
  file.close();

  digitalWrite(GREEN_PIN, LOW);

  // if unit is on batteries I flush the eeprom buffer to the SD card with a power cycle to make sure I capture that data when the unit restarts
  // long sample intervals could many mean days worth of data stored in there that I might loose when I disconnect the units
  // This routine is a huge power user! & the USB connector often can not supply current to drive it.

#ifndef ECHO_TO_SERIAL  //perhaps I need a better criterion here...dont want to loose data if I forget and leave echo to serial on?
  file.open(EPRbuffFileName, O_RDWR | O_CREAT | O_AT_END);
  file.print(F("EEprom buffer dump @:,"));
  file.println(CycleTimeStamp);
  writeHeaderInfo();
  file.close(); // make the buffer dump file if it does not exist
  LowPower.powerDown(SLEEP_30MS, ADC_OFF, BOD_OFF);
  //delay(50);  //just to give the sd some time for housekeeping...could sleep here?
  // assign FileName to EPRbuffFileName before going to the generic "flush eeprom to SD" function
  memcpy(FileName, EPRbuffFileName, 12); // see http://arduino.land/FAQ/content/6/30/en/how-to-copy-an-array.html
  flushEEpromBuffer(); //same routine that is used in the main loop, which is why we swap FileName contents
  memcpy(FileName, TempName, 12); //reset file name back to "LOG00000.CSV"
#endif
  //note: this buffer flush hits the battery pretty hard, and the A0 voltage divider capacitor may still be undercharged producing artificially low readings?

  //General Pin configurations
  //Pins 2&3 are used as interrupts  // Digital 4,5,6 used for the RGB led  // analog 4&5 being used by I2C!

  // On power-up, all pins are initialized to be a digital INPUT.  https://www.baldengineer.com/when-to-use-arduinos-pinmode-and-why.html
  // pinMode() configures a pin for use as a digital input, not analog input.
  // When analogRead() is called, it always reconfigures the Analog Pin for “input”
  // Unlike digitalRead() which can be used on both INPUT and OUTPUT, analogWrite() only works for OUTPUT
  // analogWrite() works on pins which support Pulse-Width Modulation (PWM), so it only makes sense to use it as an OUTPUT

  DIDR0 = 0x0F; // disable the digital inputs on analog 0..3 so we can use analog sensors  (analog 4&5 being used by I2C!)
  //http://jeelabs.org/2013/05/16/measuring-the-battery-without-draining-it/

  //A0 = main power supply voltage divider
  //A1 = RTC coin cell voltage divider on some units
  //A4 & A5 used for I2C
  pinMode(A6, INPUT);
  digitalWrite(A6, HIGH);
  pinMode(A7, INPUT);
  digitalWrite(A7, HIGH);

  //set UNUSED digital pins to input HIGH so they dont float during sleep
#ifndef ECHO_TO_SERIAL
  pinMode(0, INPUT); digitalWrite(0, HIGH);  //but not if we are on usb- then these pins are RX & TX
  pinMode(1, INPUT); digitalWrite(1, HIGH);
#endif
#ifndef RTCPOWER_PIN    //pin 7 used to provide power to the +Vbat on the RTC
  pinMode(7, INPUT); digitalWrite(7, HIGH); // on some builds I am using this pin to power the RTC
#endif
#ifndef TS_DS18B20
  pinMode(8, INPUT); digitalWrite(8, HIGH);  //I use this pin for the one wire bus for DS18B20s
#endif
#ifndef POWERDOWN_PIN  // dont set this if pin 9 is being used to trigger pololu power switch!
  pinMode(9, INPUT); digitalWrite(9, HIGH);
#endif
  //pins 10,11,12,13 are connected to the Sd card, SPI mode

  READSENSOR_PIN = GREEN_PIN; // the "first" powersupply read is always 50-100 mv low, and artificially sets the pip color to blue too soon
}

/*******************************************************************************************************************************************
 *******************************************************************************************************************************************
 *  *  *  *  *  *  MAIN LOOP   *  *  *  *  *  *
 *******************************************************************************************************************************************
  /******************************************************************************************************************************************/

// sensor errors during main loop should only call error routine & halt the system if ECHO_TO_SERIAL is defined (ie: we are in debug mode)
// that way if one sensor dies in the field we can still get data from the others
void loop ()
{

  // countLogs track how many records have been written to the file
  // if it goes above the fileInterval value defined at the beginning of the script, start a new file
  if (countLogs >= fileInterval) {
    digitalWrite(RED_PIN, HIGH);  //SD cards can have occasional block write times of about 200 milliseconds, and a latency of 250ms
    createLogFile();   // create a new file this is the largest power using event!
    countLogs = 0;     // reset counter for # of records logged in the same file
    digitalWrite(RED_PIN, LOW);
  }

  //CurrentPageStartAddress = 0; //resets back to start of eeprom address space (yes redundant, just making sure..)

  /*******************************************************
   * * * EVERYTHING happens inside this sub-loop! * * * *
   ******************************************************/
  /******************************************************/

  for (int Cycle = 0; Cycle < SamplesPerCycle; Cycle++) { //counts from 0 to (SamplesPerCycle-1)
    //Number of Samplespercycle is limited by the size of your the eeprom you are buffering to


    if (clockInterrupt) {  // this is redundant...just put it here to make sure the RTC alarm is off
      clearClockTrigger();
      digitalWrite(INTERRUPT_PIN, HIGH);//set weak internal pull up the interrupt pin
      //INT pin pullup is only needed on older tinyduino builds with SQW pullup removed, has no effect on other builds since they have pullup resistors in place
    }

    //-----------------------------
    // CHECK THE DATE AND THE TIME
    //-----------------------------
#if defined ADXL345_ISON
    READSENSOR_PIN = RED_PIN; //changed color on RTC initiated pip for drip sensors ONLY to distinguish it from the Drip recording pips
#endif

    digitalWrite(READSENSOR_PIN, HIGH); //Part of the the heartbeat pip for the logger
    DateTime now = RTC.now();  // Read the time and date from the RTC
    //old: sprintf(CycleTimeStamp, "%04d/%02d/%02d %02d:%02d:%02d", now.year(), now.month(), now.day(), now.hour(), now.minute(), now.second());
    //Time read always occurs <1 sec after RTC interrupt, so seconds data was always "00" - so I dont record it any more
    // sprintf ref  http://www.perlmonks.org/?node_id=20519
    sprintf(CycleTimeStamp, "%04d/%02d/%02d %02d:%02d", now.year(), now.month(), now.day(), now.hour(), now.minute());
    // changing the year to %02d had no effect on the data - it was still saved as 4 characters, so sprintf will not "crop" away excess information?
    digitalWrite(READSENSOR_PIN, LOW); // I dont leave the LED on for the sensor reads any more, as some of them have multiple readings over long intervals

    //-----------------------------------------------
    // READ VALUES FROM ANY SENSORS that are attached:
    //-----------------------------------------------

    //Read the temperature Sensor:

#ifdef TS_DS18B20
    TEMP_Raw = readDS18B20Temp(); //almost 1 second of sleeping while waiting for it's 12-bit conversion!
#endif

#if defined(TS_DS18B20)   //&& MS580X_I2C_ADDRESS?
    // I don't usually convert the temps live anymore, as the arduino introduces floating point calculation errors - better to do it later in excel
    TEMP_degC = TEMP_Raw * 0.0625;                 //do I need a negative number handler here?
    wholeTemp = (int)TEMP_degC;
    fracTemp = (TEMP_degC * 1000) - (wholeTemp * 1000); // Float split into 2 intergers so print funtions dont eat memory

#else  // If no other temperature sensor is attached, the RTC is the temperature sensor of last resort, +- 3 deg C accuracy AND trapped inside the thermal mass of the housings       
    /*	Wire.beginTransmission(DS3231_ADDRESS);
    	Wire.write(0x11); //location of Temp register MSB, LSB at 0x12
    	Wire.endTransmission();

    	Wire.requestFrom(DS3231_ADDRESS, 2);

            if(Wire.available()) {
            bytebuffer1 = Wire.read(); //2's complement int portion
            bytebuffer2 = Wire.read(); //fraction portion

            TEMP_degC = ((((short)bytebuffer1 << 8) | (short)bytebuffer2) >> 6) / 4.0; // Allows for readings below freezing - Thanks to Coding Badly
           //http://www.arduino.cc/cgi-bin/yabb2/YaBB.pl?num=1294869573/0
           //temp3231 = (temp3231 * 1.8) + 32.0; // Convert Celcius to Fahrenheit
           //also tried https://github.com/trunet/DS3231RTC with no difference
             }
             else {
             TEMP_degC = 0.0; //got no data from RTC
             }

             if(bytebuffer1 & 0b10000000) //check if -ve number  - this might be redundant with the above code?
            {
            TEMP_degC = TEMP_degC * -1.0;
            }

            wholeTemp = (int)TEMP_degC;
            fracTemp= (int)((TEMP_degC*100)-(wholeTemp*100)); // do not convert three digits or gives 0.224 & x.238 error instead of .5 & .75 fractionals
    */
#endif

    //Read the accelerometer:

    //Read the combo accel & compass:
#ifdef LSM303_ADDRESS_ACCEL
    readLSM303();
#endif

    //-------------------------------------------
    // SERIAL OUTPUT is active for debugging only
    //-------------------------------------------

#ifdef ECHO_TO_SERIAL

    Serial.print(CycleTimeStamp);
    Serial.print(F(" Cycle: "));
    Serial.print(Cycle);

    Vcc2 = readInternalVcc();// there is no external Vraw when you are on usb!
    Serial.print(F(", InternalV= "));
    Serial.print(Vcc2);
    Serial.print(F(", Coin cell(A1)V= "));
    Serial.println(CoinCellV);

#if defined(LSM303_ADDRESS_ACCEL)
    //note there is no smoothing being done to the lsm303 just using the variables to save ram
    Serial.print(F(", ACCx= "));
    Serial.print(Xa_raw);
    Serial.print(F(", ACCy= "));
    Serial.print(Ya_raw);
    Serial.print(F(", ACCz= "));
    Serial.println(Za_raw);
#endif

#if defined(LSM303_ADDRESS_MAG)
    //currently saving the raw variables...have not implemented the calibration yet...
    Serial.print(F(", MAGx= "));
    Serial.print(Xm_raw);
    Serial.print(F(", MAGy= "));
    Serial.print(Ym_raw);
    Serial.print(F(", MAGz= "));
    Serial.println(Zm_raw);
#endif

    wholeTemp = (tiltAngle / 100); //just subbing in tilt angle calculation here
    fracTemp = tiltAngle - (wholeTemp * 100); // Float split into 2 intergers so print funtions dont eat memory
    Serial.print(F("TiltAngle: "));
    Serial.print(wholeTemp);
    Serial.print(F("."));
    Serial.print(fracTemp); //avoid sending floats to serial print - it eats sram!

    /*  never got the lm303 temp sensor working properly!
      #ifdef LSM303_ADDRESS_ACCEL  // only enable this for debugging, will rarely use the internal temp from the LSM303?
        TEMP_degC = (float)(LSM303_TEMP_Raw/8.0);                    //do I need a negative number handler here?
        wholeTemp = (int)TEMP_degC;
        fracTemp= (TEMP_degC*1000) - (wholeTemp*1000); // Float split into 2 intergers so print funtions dont eat memory
        Serial.print(F(", LSM303 Temp C: "));
        Serial.print(wholeTemp);
        Serial.print(F("."));
        Serial.print(fracTemp); //avoid sending floats to serial print - it eats sram!
      #endif
    */
    Serial.print(F(", Free Ram: "));
    Serial.println(freeRam()); //only use this for debugging
    Serial.println(""); //terminate line

    Serial.flush(); //to clear serial com lines
#endif

    /*****************************************************************************
     * * * LOAD SENSOR DATA INTO CHARACTER STRINGS & then BUFFER TO EEPROM * * *
     *****************************************************************************/

    // 28 bytes AVAILIABLE in the 32byte page, PString allows end of buffer to be filled with blank spaces without overflow
    // you could also contruct these buffers with sprintf statements if you new exactly what your data looked like (as I did with the Timestamp)

    digitalWrite(READSENSOR_PIN, HIGH); //Use this as the Normal heartbeat pip for flow sensors  Each eeprom page write takes 5ms

    PagesBuffered2Eeprom = 0; // used by flushEeprombuffer function! this number must match the number of "pages" you use to store each record's worth of sensor data!
    PString str(EEPROMBuffer, sizeof(EEPROMBuffer));

    // The 1st character string always contains the record time stamp!
    //-----------------------------------------------------------------
    str = CycleTimeStamp;           //17 / 16 characters without seconds plus comma
    str.print(F(","));
    str.print(PSvolts);    //4 charaters to track power supply

#if defined ADXL345_ISON  //drip sensors only use the internal RTC temp
    str.print(F(","));
    str.print(CoinCellV);    //lots of room in second string of drip counters
    //str.print(F(",")); str.print(Vcc2);   //4 charaters to track RTC Coin Cell voltage
#else
    str.print(F(","));
    str.print(TEMP_Raw);  //all are 12 bit sensors, so 4095 is largest value =4 charaters  // TEMP_degC =TEMP_Raw*0.0625;
#endif
    str.print(F("         "));  // just filler spaces for the end of the string to accomodate changes in data size

    Write_i2c_eeprom_page(EEPROM_ADDRESS, CurrentPageStartAddress, EEPROMBuffer); // whole page is written at once here
    CurrentPageStartAddress += EEPromPageSize; PagesBuffered2Eeprom++;  // used by flushEeprombuffer function!

    //Construct 2nd char string of 28 bytes
    //----------------------------------------
    str = ",";
#if defined(LSM303_ADDRESS_ACCEL) // 20 characters of data incl signs commas for bma180, less for bma250
    //currently saving the raw variables...have not implemented the calibration yet...
    str.print(Xa_raw);          // 6  5 or 6char:   +_1023 from BMA250  OR (+- 16,384) from BMA180
    str.print(F(","));
    str.print(Ya_raw);
    str.print(F(","));
    str.print(Za_raw);
    str.print(F(","));
    // here I am using the temperature variables to put tiltAngle into the string
    wholeTemp = (tiltAngle / 100); //just subbing in tilt angle calculation here
    fracTemp = tiltAngle - (wholeTemp * 100); // Float split into 2 intergers so print funtions dont eat memory
    str.print(wholeTemp);  //two digits positive
    str.print(F("."));
    str.print(fracTemp);   //2 digits, because *1000, but the third digit is just junk data?
    //str.print(PSvolts);
#endif
    str.print(F("                         "));

    Write_i2c_eeprom_page(EEPROM_ADDRESS, CurrentPageStartAddress, EEPROMBuffer); // 28 bytes/page is max whole page is written at once here
    CurrentPageStartAddress += EEPromPageSize; PagesBuffered2Eeprom++;

    //Construct 3rd char string of 28 bytes IF needed
    //--------------------------------------------------

#if defined(HMC5883_ADDRESS) || defined(HTDU21D_ADDRESS) || defined(LSM303_ADDRESS_MAG) // RH or compass data means we need to construct a third string
    str = ",";

#if defined(HMC5883_ADDRESS) || defined(LSM303_ADDRESS_MAG)
    //currently saving the raw variables...have not implemented the calibration yet...
    str.print(Xm_raw);  //max 18 characters from compass, usually less
    str.print(F(","));
    str.print(Ym_raw);
    str.print(F(","));
    str.print(Zm_raw);
    str.print(F(","));
#endif
    // you only have RAM for MS5803 1st calcs if there is no compass attached!!!!

    str.print(CoinCellV); //this will be clipped most of the time with the RH sensor
    str.print(F(",")); // put a comma before the filler spaces on the LAST BUFFER STRING ONLY
    str.print(F("                  "));


    Write_i2c_eeprom_page(EEPROM_ADDRESS, CurrentPageStartAddress, EEPROMBuffer); // 28 bytes/page is max whole page is written at once here
    CurrentPageStartAddress += EEPromPageSize; PagesBuffered2Eeprom++;

#endif

    // Following the pattern above,you can create as many buffer strings as you need to store your sensor data in the eeprom, provided SamplesPerCycle is updated accordingly.
    // Maximum value of Samples per cycle = # of 32byte pages your eeprom can hold / PagesBuffered2Eeprom (which is the number of pages you need per record)

    digitalWrite(READSENSOR_PIN, LOW);

#if defined ADXL345_ISON
    READSENSOR_PIN = GREEN_PIN;
#endif


    /*****************************************
      If full, FLUSH EEPROM BUFFER TO SD CARD
     *****************************************/
    // Check if you are on the last cycle (so eeprom is full) and if so run a loop to dump data to the sd card
    // BUT only do this if PSvolts is above CutoffVoltage so there is enough power to write to the SD card safely

    if (Cycle == (SamplesPerCycle - 1) && PSvolts >= CutoffVoltage) {

      flushEEpromBuffer();

    }

    /***********************************************************
      SET THE NEXT RTC ALARM and go back to sleep
     ***********************************************************/

    Alarmhour = now.hour();
    Alarmminute = now.minute() + SampleIntervalMinutes;
    Alarmday = now.day();
    if (SampleIntervalMinutes > 0) //then our alarm is in (SampleInterval) minutes
    {
      if (Alarmminute > 59) {  //error catch - if alarmminute=60 the interrupt never triggers due to rollover!
        Alarmminute = 0;
        Alarmhour = Alarmhour + 1;
        if (Alarmhour > 23) {
          Alarmhour = 0;

          // Code placed here will bet run ONCE-PER-DAY - at the midnight rollover,
          // Like checking how the Battery pack is doing!
          readCoinCell();  // Check RTC coin cell once per day -must be in front of readExternalPSvolts so analog pin cap does not stay connected to that divider
          PSvolts = readExternalPSvolts();  // Check psupply at least once per day
          // or reading the position of your acclerometers into the "DayEventBuffer" which only saved after a flushEEprombuffer event
#ifdef ADXL345_ISON
          readAdxl345(); //I read the accelerometer xyz once per day to make sure the drip sensor has not been moved or fallen over
          PString str(DayEventBuffer, sizeof(DayEventBuffer)); //DayEventBuffer stores these readings till the next SD card writing event, so may be out of place in the data file depending on # of days data buffered.
          str.print(F(",,,,,")); //this pushes the data to the right on the spreadsheet for easier sorting/filtering later
          str.print(smoothACCx);
          str.print(F(","));
          str.print(smoothACCy);
          str.print(F(","));
          str.print(smoothACCz);
          str.print(F(","));
          str.print(Alarmday);  // we only have 28 characters in DayEventBuffer, so only room for the "day" of the reading here
          //  now.month(), now.day()?
#endif
        }
      }
      // comment this line out for rapid calibration
      // RTC.setAlarm1Simple(Alarmhour, Alarmminute);
    }

    else  // for testing and debug I sometimes want the alarms more frequent than 1 per minute - that is the only time this code is used

    {
      Alarmsecond = now.second() + SampleIntSeconds;
      if (Alarmsecond > 59) {
        Alarmsecond = 0;
        Alarmminute = Alarmminute + 1;
        if (Alarmminute > 59)
        { //error catch - if alarmminute=60 the interrupt never triggers due to rollover!
          Alarmminute = 0;
          Alarmhour = Alarmhour + 1;
          if (Alarmhour > 23) { //uhoh a day rollover, but we dont know the month..so we dont know the next day number?
            Alarmhour = 0;

            // CAN place ONCE-PER-DAY events in the code right here!

            // Like checking how the Battery pack is doing!
            PSvolts = readExternalPSvolts();  // Check psupply to make sure its ok to write to sd card

#ifdef ADXL345_ISON
            readAdxl345(); //I read the accelerometer xyz once per day to make sure the drip sensor has not moved or fallen over
            PString str(DayEventBuffer, sizeof(DayEventBuffer));
            str.print(F(",,,,,"));
            str.print(smoothACCx);
            str.print(F(","));
            str.print(smoothACCy);
            str.print(F(","));
            str.print(smoothACCz);
            str.print(F(","));
            str.print(Alarmday);  // time will get clipped in this 28 character buffer, but I already know this record is only generated at midnight
#endif

            // sleep for a total of 64 seconds (12 x 8s) so the day "rolls over" while we are in this loop
            // this causes a gap in the timing, but I only use sub minute sampling for debug anyway.

            for (int j = 0; j < 12; j++) {
              LowPower.powerDown(SLEEP_8S, ADC_OFF, BOD_OFF);
            }

            //or you could just wait for a 1.5 minutes to pass..
            //delay(327670);// Just wait for a 1.5 minutes to pass..
            //delay(327670);
            //delay(327670);

            DateTime now = RTC.now();  //now set the alarm again with the day already rolled over
            Alarmday = now.day();
            Alarmhour = now.hour();
            Alarmminute = now.minute();
            Alarmsecond = now.second() + SampleIntSeconds;
          }
        }
      }
      RTC.setA1Time(Alarmday, Alarmhour, Alarmminute, Alarmsecond, 0b00001000, false, false, false);
      //The variables ALRM1_SET bits and ALRM2_SET are 0b1000 and 0b111 respectively.
      //setA1Time(byte A1Day, byte A1Hour, byte A1Minute, byte A1Second, byte AlarmBits, bool A1Dy, bool A1h12, bool A1PM)
    }
    RTC.turnOnAlarm(1);
    delay(2);  //give the RTC a few ms to finish operations

#ifdef ECHO_TO_SERIAL
    Serial.println("");
    Serial.print(F("  Alarm Set:"));
    Serial.print(now.hour(), DEC);
    Serial.print(':');
    Serial.print(now.minute(), DEC);
    Serial.print(F(" Sleep:"));
    Serial.print(SampleIntervalMinutes);
    Serial.print(F(" min."));
    Serial.print(SampleIntSeconds);
    Serial.println(F(" sec."));
    Serial.println("");
    Serial.flush();
    //delay(50); //a delay long enought to boot out the serial coms
#endif

    //---------------------------------------------------------------------------------------------------------------------------
    //the following DRIP SENSOR ONLY loop continues updating the drip counter ("tapcount" in the sleepNwait4AccInterrupt() routine)
    //until a RTC interrupt occurs, which breaks out because clockInterrupt = true;
    //none of the other datalogger configurations use this ADXL345 little loop!
    //-------------------------------------------------------------------------------------------------------------------------------


    // comment out sleepNwait4RTC();  line out for rapid calibration OUTPUT
    //sleepNwait4RTC();  //sleep and wait for RTC ONLY -  call is inside the main cycle counter loop */
    delay(250);

  }//samples per CYCLE LOOP TERMINATOR (# of sample cycles buffered in eeprom before sd card write happens)

}//the MAIN void LOOP TERMINATOR

/*******************************************************************************************************************************************
 *******************************************************************************************************************************************
 *  *  *  *  *  *  END OF MAIN LOOP   *  *  *  *  *  *
 *******************************************************************************************************************************************
  /******************************************************************************************************************************************/

//http://www.st.com/web/en/resource/technical/document/application_note/CD00269797.pdf  Using LSM303DLH for a tilt compensated electronic compass - hard to interpret

//pololus LM303 software:  https://github.com/pololu/lsm303-arduino  might work, but they have some kind of autodetect for their breakout boards
//"vector" is something they constructed in the .h file  http://forum.arduino.cc/index.php/topic,45626.0.html
// forum discussion http://forum.pololu.com/viewtopic.php?f=3&t=5567
// based on their imu http://www.hobbytronics.co.uk/minimu-9
// video for compass calib using pololu calibrate file https://www.youtube.com/watch?v=h7gElYWgt0k
//http://www.instructables.com/id/LSM303DLHC-exploration/step2/Calibration/

// An approach to calibration for this sensor - uses the pololu libraries
// http://forum.arduino.cc/index.php?topic=265541.0
// uses magneto from https://sites.google.com/site/sailboatinstruments1/home
/*
  // http://forum.arduino.cc/index.php?topic=194711.0
  The "low and high limit" method of calibrating an accelerometer doesn't work very well.
  My favorite method involves collecting a bunch of accelerometer or magnetometer data points
  with the device in different orientations, then using a program on a PC to convert the resulting
  3-D ellipsoid into a sphere.
  Here is a great intro to the technique: http://sailboatinstruments.blogspot.com/2011/08/improved-magnetometer-calibration.html

  The blog describes magnetometer calibration but the same procedure works just as well for the accelerometer,
  provided that the device is held still while each data point is recorded. Once calibrated, 1 "g" is 9.8 m/s^2
  so the conversion factor for the accelerometer is trivial to calculate.
*/

// also see https://github.com/pololu/minimu-9-ahrs-arduino
// Pololu MinIMU-9 + Arduino AHRS (Attitude and Heading Reference System)
// specifically the heading calculation at:
// https://github.com/pololu/minimu-9-ahrs-arduino/blob/master/MinIMU9AHRS/Compass.ino

#ifdef LSM303_ADDRESS_ACCEL
// datasheet: http://www.adafruit.com/datasheets/LSM303DLHC.PDF
//  https://github.com/adafruit/Adafruit_LSM303DLHC/blob/master/Adafruit_LSM303_U.cpp  more recent code? based on Adafruit's Unified Sensor Library (Adafruit_Sensor).
//  http://developer.mbed.org/users/Spilly/code/LSM303DLHC/file/386547b2ee70/LSM303DLHC.cpp  for another code example with floats

#define LSM303_REGISTER_ACCEL_CTRL_REG1_A 0x20 // default 00000111  rw
#define LSM303_REGISTER_ACCEL_OUT_X_L_A   0x28
#define LSM303_REGISTER_ACCEL_CTRL_REG4_A 0x23

#define LSM303_REGISTER_MAG_MR_REG_M    0x02
#define LSM303_REGISTER_MAG_OUT_X_H_M   0x03
#define LSM303_REGISTER_MAG_CRA_REG_M   0x00
#define LSM303_REGISTER_MAG_CRB_REG_M   0x01
#define LSM303_REGISTER_MAG_TEMP_OUT_H_M 0x31
#define LSM303_REGISTER_MAG_TEMP_OUT_L_M 0x32

// other lsm303AccelRegisters
// from https://github.com/adafruit/Adafruit_LSM303/blob/master/Adafruit_LSM303.h
// for more registers see: https://github.com/pololu/lsm303-arduino/blob/master/LSM303/LSM303.h


//LM303 Magnetic and accelerometer parts can be enabled or put into power-down mode separately

//The accelerometer's output register is just 12 bits and not 16 bits, so you need to right-shift the value by 4 and multiply it with 0,001 G. Furthermore it's little-endian.
/*
  float lsm303AccelX;  //Why floats? this is all integer data?
  float lsm303AccelY;
  float lsm303AccelZ;
  float lsm303MagX;
  float lsm303MagY;
  float lsm303MagZ;
*/


void initLSM303(void)  {

  // Set Acc Output Data Rate      CTRL_REG1_A = [ODR3|ODR2|ODR1|ODR0|LowPowerEnable|Zen|Yen|Xen]
  i2c_writeRegByte(LSM303_ADDRESS_ACCEL, LSM303_REGISTER_ACCEL_CTRL_REG1_A, 0b00100111); //=ODR of 10Hz
  // ODR =0000=Power-down mode; LowPowren=0=(normal mode); Zen = Yen = Xen = 1 (all axes enabled)

  // Set Acc Scale & resolution         FS = 00 (+/- 2 g full scale); HR = 1 (high resolution enable)
  i2c_writeRegByte(LSM303_ADDRESS_ACCEL, LSM303_REGISTER_ACCEL_CTRL_REG4_A, 0b00001000);
  // CTRL_REG4_A = [BDU|Big/little endian| FS1 |FS0 |HR | 0 | 0 |SIM]  //FS1 FS0= (00: +/- 2G, 01: +/- 4G, 10: +/- 8G, 11: +/- 16G)

  // set Mag Output Data Rate         CRA_REG_M = [ TEMP_EN  |0|0|  DO2 | DO1 | DO0 |0|0 ] // Temp_en=1 to enable
  i2c_writeRegByte(LSM303_ADDRESS_MAG, LSM303_REGISTER_MAG_CRA_REG_M, 0b10001100); //=7.5Hz
  //D2-D0 :  000=0.75Hz, 001=1.5, 010=3.0, 011=7.5,100=15, 101=30Hz

  // set Mag Gain  CRB_REG_M = [GN2,GN1,GN0 |0|0|0|0|0 ] last 5 must be zeros    // output range = -2048 to 2047 for 2g, so only +-1023 in tilt sensing application
  i2c_writeRegByte(LSM303_ADDRESS_MAG, LSM303_REGISTER_MAG_CRB_REG_M, 0b00100000); // +/- 1.3 gauss (this is the default range)
  //what should I set the gain to? https://forum.sparkfun.com/viewtopic.php?t=18510

  /*
        LSM303_MAGGAIN_1_3 = 0x20,  // +/- 1.3   // 0x20 = 0b00100000  // GN = 001 (+/- 1.3 gauss full scale)
        LSM303_MAGGAIN_1_9 = 0x40,  // +/- 1.9
        LSM303_MAGGAIN_2_5 = 0x60,  // +/- 2.5
        LSM303_MAGGAIN_4_0 = 0x80,  // +/- 4.0
        LSM303_MAGGAIN_4_7 = 0xA0,  // +/- 4.7
        LSM303_MAGGAIN_5_6 = 0xC0,  // +/- 5.6
        LSM303_MAGGAIN_8_1 = 0xE0   // +/- 8.1
  */

  // Enable the magnetometer  MR_REG_M = [0|0|0|0|0|0| MD1 | MD0 ]
  i2c_writeRegByte(LSM303_ADDRESS_MAG, LSM303_REGISTER_MAG_MR_REG_M, 0x00);
  // MD = 00 (continuous-conversion mode); MD=01 (single conversion mode) ; MD 10 or 11 = Sleep-Mode  0b00000011

}

void readLSM303() {  //no averaging loop like the bma180 yet, just one shot reading

  // Wake up the Accelerometer & set ODR            CTRL_REG1_A = [ODR3|ODR2|ODR1|ODR0|LowPowerEnable|Zen|Yen|Xen]
  i2c_writeRegByte(LSM303_ADDRESS_ACCEL, LSM303_REGISTER_ACCEL_CTRL_REG1_A, 0b00100111); //=ODR of 10Hz in the
  // ODR =0000=Power-down mode; LowPowren=0=(normal mode); Zen = Yen = Xen = 1 (all axes enabled)

  // Mag set ODR          CRA_REG_M = [ TEMP_EN  |0|0|  DO2 | DO1 | DO0  |0|0 ]
  i2c_writeRegByte(LSM303_ADDRESS_MAG, LSM303_REGISTER_MAG_CRA_REG_M, 0b00001100); //=7.5Hz

  // Wake Up the magnetometer by turning on continuous mode
  i2c_writeRegByte(LSM303_ADDRESS_MAG, LSM303_REGISTER_MAG_MR_REG_M, 0x00);

  // Give the registers time to fill with new data - skip the first readings
  LowPower.powerDown(SLEEP_250MS, ADC_OFF, BOD_ON); //give it some time to fill the registers with new data

  // Read the accelerometer
  Wire.beginTransmission((byte)LSM303_ADDRESS_ACCEL);
  Wire.write(LSM303_REGISTER_ACCEL_OUT_X_L_A | 0b10000000); //asserting the most significant bit of the register address
  // 7 LSBs represent the actual register address while the MSB enables address autoincrement.
  // If the MSB of the SUB field is ‘1’, the SUB (register address) is automatically increased to allow multiple data Read/Write.
  Wire.endTransmission();
  Wire.requestFrom((byte)LSM303_ADDRESS_ACCEL, (byte)6);

  // Wait around until enough data is available  //dont need this with the 250ms delay we put in
  // while (Wire.available() < 6);  (X|Y|Z)

  uint8_t xlo = Wire.read();
  uint8_t xhi = Wire.read();
  uint8_t ylo = Wire.read();
  uint8_t yhi = Wire.read();
  uint8_t zlo = Wire.read();
  uint8_t zhi = Wire.read();

  // Shift values to create properly formed integer (low byte first)
  Xa_raw = (xlo | (xhi << 8)) >> 4;
  Ya_raw = (ylo | (yhi << 8)) >> 4;
  Za_raw = (zlo | (zhi << 8)) >> 4;

  /*  //why >>4? from https://github.com/pololu/lsm303-arduino/blob/master/LSM303/examples/Serial/Serial.ino
    In the LSM303DLHC, LSM303DLM, and LSM303DLH, the 16 bit acceleration data
    registers actually contain a left-aligned 12-bit number, so the lowest
    4 bits are always 0, and the values should be shifted right by 4 bits
    (divided by 16) to be consistent with the conversion factors specified
    in the datasheets. (page 11) states a conversion factor of 1 mg/digit
    at this FS setting, so the value of -1009 corresponds to -1009 * 1 =
    1009 mg = 1.009 g.
  */

  // Sleep the accelerometer
  i2c_writeRegByte(LSM303_ADDRESS_ACCEL, LSM303_REGISTER_ACCEL_CTRL_REG1_A, 0b00000111);
  // ODR =0000=Power-down mode; LowPowren=0=(normal mode); Zen = Yen = Xen = 1 (all axes enabled)

  // Read the magnetometer
  Wire.beginTransmission((byte)LSM303_ADDRESS_MAG);
  Wire.write(LSM303_REGISTER_MAG_OUT_X_H_M | 0b10000000);  //added the pipe here myself
  Wire.endTransmission();
  Wire.requestFrom((byte)LSM303_ADDRESS_MAG, (byte)6);

  // Wait around until enough data is available
  // while (Wire.available() < 6);

  // Note high before low (different than accel) (X|Z|Y)
  xhi = Wire.read();
  xlo = Wire.read();
  zhi = Wire.read();
  zlo = Wire.read();
  yhi = Wire.read();
  ylo = Wire.read();

  // Shift values to create properly formed integer (low byte first)
  Xm_raw = (xlo | (xhi << 8));
  Ym_raw = (ylo | (yhi << 8));
  Zm_raw = (zlo | (zhi << 8));
  //To calculate the value you need to dIvide X and Y by 980 gauss⁻¹, it's 1100 gauss⁻¹ for Z

  // Sleep the magnetometer
  i2c_writeRegByte(LSM303_ADDRESS_MAG, LSM303_REGISTER_MAG_MR_REG_M, 0b00000011); // MD10 or 11 = Sleep-Mode  0b00000011

  //Now do calculations for theta & tilt direction
  // watch out for problems over-running your variables! http://forum.arduino.cc/index.php?topic=57057.0

  //The projection of the gravitational pull vector onto the xy plane
  //g' = ||g'|| = sqrt(gx^2 + gy^2) = Gproj_2_XYplane

  // tilt = angle_z = arctan(Gproj_2_XYplane / z component) // radians
  // (180.0 * atan2 (sqrt (x*x+y*y), z) / PI) ;  // degrees
  Gproj_2_XYplane = sqrt(sq((long)Xa_raw) + sq((long)Ya_raw)); //sqrt (x*x+y*y) gives the horizontal component, z is the vertical component
  // needs typecast to longs or overrunns the ints  http://forum.arduino.cc/index.php?topic=57057.0
  tiltAngle = atanInt(Gproj_2_XYplane, Za_raw); //returns tilt angle in degrees*100 (as an int)

  /*********************
   * Begin Abby's changes
   * 
   * 1. Calculate tilt angle
   * *******************/

}


#endif

// ************************************************************************************************************
// ************************************************************************************************************
// The following functions from Comp6DOF_n0m1 Library
// Author  : Noah Shibley, Michael Grant, NoMi Design Ltd.  n0m1.com
// from https://github.com/n0m1/Comp6DOF_n0m1/blob/master/Comp6DOF_n0m1.cpp
// I did not use their entire library because 1) I ran out of ram, and 2) I am doing my hard iron offset calcs with
// sailboat instruments Magcal on an LSM303dlhc
// ************************************************************************************************************
// ************************************************************************************************************

/***********************************************************

   compCompass
   Ported to C from the Freescale appnote AN4248.
   http://www.freescale.com/files/sensors/doc/app_note/AN4248.pdf
   The sine function comes from Dave Dribin's TrigInt lib.
   https://bitbucket.org/ddribin/trigint

 ***********************************************************/

/*Raw values or scaled doesn't matter, raw is faster and more accurate.In process, the
  library scales everything up with an equal multiplier to reduce quantization losses.
  You may want to play with the arrangement and sign of xyz depending on the orientation
  of your accelerometer and compass.

  This application note uses the industry standard “NED” (North, East, Down) coordinate system to label
  axes on the mobile phone. The x-axis of the phone is the eCompass pointing direction, the y-axis points to
  the right and the z-axis points downward.

  A positive yaw angle ψ is defined to be a clockwise rotation about the positive z-axis (clockwise looking down on the sensor). Similarly, a positive
  pitch angle θ and positive roll angle φ are defined as clockwise rotations about the positive y- and positive
  x-axes respectively.

  Since Equations have an infinite number of solutions at multiples of 360°, it is standard convention to restrict
  the solutions for roll, pitch and yaw to the range -180° to 180°. A further constraint is imposed on the pitch angle
  to limit it to the range -90° to 90°. This ensures only one unique solution exists for the compass, pitch and roll
  angles for any orientation. Equations for roll and yaw angle are therefore computed with a software ATAN2 function
  (with output angle range -180° to 180°) and pitch is computed with a software ATAN function (with output angle range -90° to 90°).
*/

void compCompass(int xMagAxis, int yMagAxis, int zMagAxis, int xAccel, int yAccel, int zAccel, boolean lowpass)
{

  int sinVal, cosVal;         /* sine and cosine */

  /* calculate current roll angle Phi */
  //roll_ = atan2Int(yAccel, zAccel); //default from n0m1
  roll_ = atan2Int(-yAccel, (sqrt(sq((long)xAccel) + sq((long)zAccel))) ); //modified to match avrfreaks

  /*
    //http://www.avrfreaks.net/forum/accelerometer-roll-and-pitch?page=all
    use
    pitch = atan2(ax,sqrt((ay*ay)+(az*az))) and
    roll = atan2(-ay,-sqrt((ax*ax)+(az*az)))
    instead of the simpler formulae of:
    pitch = atan2(ax,az) and
    roll = atan2(-ay,-az)
  */

  /* calculate sin and cosine of roll angle Phi */
  unsigned int angle = degToAngle((roll_ / 100) + 180);
  sinVal = sinInt(angle);

  int cosAngle = atan2Int(zAccel, yAccel);
  angle = degToAngle((cosAngle / 100) + 180);
  cosVal = sinInt(angle);

  /* de-rotate by roll angle Phi */
  yMagAxisComp_ = (int)(((unsigned long)yMagAxis * cosVal - (unsigned long)zMagAxis * sinVal) >> 15);/* Eq 19 y component */
  zMagAxis = (int)(((unsigned long)yMagAxis * sinVal + (unsigned long)zMagAxis * cosVal) >> 15);/* Bpy*sin(Phi)+Bpz*cos(Phi)*/
  zAccel = (int)(((unsigned long)yAccel * sinVal) + ((unsigned long)zAccel * cosVal) >> 15);/* Eq 15 denominator */

  /* calculate current pitch angle Theta */
  //pitch_ = atan2Int((int)-xAccel, zAccel); //n0m1 default /* Eq 15 */
  pitch_ = atan2Int(xAccel, (sqrt(sq((long)yAccel) + sq((long)zAccel))) ); //avrfreaks
  // alternate pitch = atan2(ax,sqrt((ay*ay)+(az*az)))
  /* restrict pitch angle to range -90 to 90 degrees */
  if (pitch_ > 9000) pitch_ = (int) (18000 - pitch_);
  if (pitch_ < -9000) pitch_ = (int) (-18000 - pitch_);

  /* calculate sin and cosine of pitch angle Theta */
  int sinAngle = atan2Int(xAccel, zAccel);
  angle = degToAngle((sinAngle / 100) + 180);
  sinVal = sinInt(angle);

  cosAngle = atan2Int(zAccel, xAccel);
  angle = degToAngle((cosAngle / 100) + 180);
  cosVal = sinInt(angle);

  /* correct cosine if pitch not in range -90 to 90 degrees */
  if (cosVal < 0) cosVal = (int) - cosVal;

  /* de-rotate by pitch angle Theta */
  xMagAxisComp_ = (int)(((unsigned long)xMagAxis * cosVal + (unsigned long)zMagAxis * sinVal) >> 15); /* Eq 19: x component */
  zMagAxisComp_ = (int)(((unsigned long) - xMagAxis * sinVal + (unsigned long)zMagAxis * cosVal) >> 15); /* Eq 19: z component */

  /* calculate current yaw = e-compass angle Psi = heading */
  yaw_ = atan2Int((int) - yMagAxisComp_, xMagAxisComp_); /* Eq 22 */
  //yaw_ = atan2Int((int)yMagAxisComp_, xMagAxisComp_);  //modify sign on h_y
  // also see http://www.timzaman.com/2011/04/heading-calculating-heading-with-tilted-compass/

  if (lowpass == true)
  {
    lpRoll_ = lowPassInt(roll_, lpRoll_, -180, 180);
    lpPitch_ = lowPassInt(pitch_, lpPitch_, -90, 90);
    lpYaw_ = lowPassInt(yaw_, lpYaw_, -180, 180);

    roll_ = lpRoll_;
    pitch_ = lpPitch_;
    yaw_ = lpYaw_;
  }

}


/***********************************************************

   degToAngle

 ***********************************************************/
unsigned int degToAngle(int degrees)
{
  unsigned int angle = (unsigned int)(((unsigned long)degrees * TRIGINT_ANGLES_PER_CYCLE) / 360);
  return angle;
}

/***********************************************************

   atan2Int

 	The function Atan2Int is a wrapper function which
 	implements the ATAN2 function by assigning the results
 	of an ATAN function to the correct quadrant. The result
 	is the angle in degrees times 100. Calculates
 	100*atan2(y/x)=100*atan2(y,x) in deg for x, y in range
 	-32768 to 32767

 ***********************************************************/
int atan2Int(int y, int x)
{

  int result;    /* angle in degrees times 100 */

  /* check for -32768 which is not handled correctly */
  if (x == -32768) x = -32767;
  if (y == -32768) y = -32767;

  /* check for quadrants */
  if ((x >= 0) && (y >= 0))  /* range 0 to 90 degrees */
  {
    result = atanInt(y, x);
  }
  else if ((x <= 0) && (y >= 0))  /* range 90 to 180 degrees */
  {
    result = (int)(18000 - (int)atanInt(y, (int) - x));
  }
  else if ((x <= 0) && (y <= 0))  /* range -180 to -90 degrees */
  {
    result = (int)((int) - 18000 + atanInt((int) - y, (int) - x));
  }
  else /* x >=0 and y <= 0 giving range -90 to 0 degrees */
  {
    result = (int)(-atanInt((int) - y, x));
  }

  return result;

}

/***********************************************************

   atanInt

 	The function iHundredAtanDeg computes the atan function
 	for X and Y in the range 0 to 32767 calculates
 	100*atan(y/x) range 0 to 9000 for all x, y positive in
 	range 0 to 32767

 ***********************************************************/
int atanInt(int y, int x)
{

  long angle;   /* angle in degrees times 100 */
  int ratio;   /* ratio of y / x or vice versa */
  long tmp;     /* temporary variable */

  /* check for pathological cases */
  if ((x == 0) && (y == 0)) return (0);
  if ((x == 0) && (y != 0)) return (9000);

  /* check for non-pathological cases */
  if (y <= x)
  {
    ratio = divInt(y, x); /* return a fraction in range 0. to 32767 = 0. to 1. */
  }
  else
  {
    ratio = divInt(x, y); /* return a fraction in range 0. to 32767 = 0. to 1. */
  }

  /* first, third and fifth order polynomial approximation */
  angle = (long) K1 * (long) ratio;
  tmp = ((long) ratio >> 5) * ((long) ratio >> 5) * ((long) ratio >> 5);
  angle += (tmp >> 15) * (long) K2;
  tmp = (tmp >> 20) * ((long) ratio >> 5) * ((long) ratio >> 5);
  angle += (tmp >> 15) * (long) K3;
  angle = angle >> 15;

  /* check if above 45 degrees */
  if (y > x) angle = (int)(9000 - angle);

  /* for tidiness, limit result to range 0 to 9000 equals 0.0 to 90.0 degrees */
  if (angle < 0) angle = 0;
  if (angle > 9000) angle = 9000;

  return ((int) angle);

}

/***********************************************************

   divInt

 	The function divInt is an accurate integer division
 	function where it is given that both the numerator and
 	denominator are non-negative, non-zero and where the
 	denominator is greater than the numerator. The result
 	is in the range 0 decimal to 32767 decimal which is
 	interpreted in Q15 fractional arithmetic as the range
 	0.0 to 0.9999695.function to calculate r = y / x with
 	y <= x, and x, y both > 0

 ***********************************************************/
int divInt(int y, int x)

{

  int tmp;               /* scratch */
  int r;                 /* result = y / x range 0., 1. returned in range 0 to 32767 */
  int delta;             /* delta on candidate result dividing each stage by factor of 2 */

  /* set result r to zero and binary search step to 16384 = 0.5 */
  r = 0;
  delta = 16384;                      /* set as 2^14 = 0.5 */

  /* to reduce quantization effects, boost x and y to the maximum signed 16 bit value */
  while ((x < 16384) && (y < 16384))
  {
    x = (int)(x + x);
    y = (int)(y + y);
  }

  /* loop over binary sub-division algorithm solving for ir*x = y */
  do
  {
    /* switch casts to int if everything works */
    unsigned long temper = (int)(r + delta);       /* itmp=ir+delta, the candidate solution */
    temper = (int)((temper * x) >> 15);
    tmp = (int)temper;

    if (tmp <= y) r += delta;

    delta = (int)(delta >> 1);     /* divide by 2 using right shift one bit */


  } while (delta >= MINDELTADIV);   /* last loop is performed for delta=MINDELTADIV */

  return (r);

}

/***********************************************************

    lowPassInt

    The code is written for filtering the yaw (compass) angle
 	ψ but can also be used for the roll angle.For the pitch
 	angle θ, which is restricted to the range -90° to 90°,
 	the final bounds check should be changed

 ***********************************************************/

int lowPassInt(int input, int prevOut, int minBound, int maxBound)
{
  long tmpAngle;          /* temporary angle*100 deg: range -36000 to 36000 */
  int output = prevOut;             /* low pass filtered angle*100 deg: range -18000 to 18000 */
  unsigned int ANGLE_LPF = 32768 / 8; /* low pass filter: set to 32768 / N for N samples averaging */

  /* implement a modulo arithmetic exponential low pass filter on the yaw angle */
  /* compute the change in angle modulo 360 degrees */
  tmpAngle = (long)input - (long)output;

  if (tmpAngle > 18000) tmpAngle -= 36000;
  if (tmpAngle < -18000) tmpAngle += 36000;

  /* calculate the new low pass filtered angle */
  tmpAngle = (long)output + ((ANGLE_LPF * tmpAngle) >> 15);


  /* check that the angle remains in -180 to 180 deg bounds */
  if (maxBound == 180)
  {
    if (tmpAngle > 18000) tmpAngle -= 36000;
    if (tmpAngle < -18000) tmpAngle += 36000;
  }
  else if (maxBound == 90)
  {
    if (tmpAngle > 9000) tmpAngle = (int) (18000 - tmpAngle);
    if (tmpAngle < -9000) tmpAngle = (int) (-18000 - tmpAngle);
  }

  /* store the correctly bounded low pass filtered angle */
  return output = (int)tmpAngle;

}

/***********************************************************

   sinInt

 ***********************************************************/

int sinInt(unsigned int angle)
{
  angle += SINE_ROUNDING;
  long interp = BITS(angle, SINE_INTERP_WIDTH, SINE_INTERP_OFFSET);
  byte index = BITS(angle, SINE_INDEX_WIDTH, SINE_INDEX_OFFSET);

  bool isOddQuadrant = (angle & QUADRANT_LOW_MASK) == 0;
  bool isNegativeQuadrant = (angle & QUADRANT_HIGH_MASK) != 0;

  if (!isOddQuadrant) {
    index = SINE_TABLE_SIZE - 1 - index;
  }

  // Do calculations with 32 bits since the multiplication can overflow 16 bits
  long x1 = sin16LUT(index);
  long x2 = sin16LUT(index + 1);
  long approximation = ((x2 - x1) * interp) >> SINE_INTERP_WIDTH;

  int sine;
  if (isOddQuadrant) {
    sine = x1 + approximation;
  } else {
    sine = x2 - approximation;
  }

  if (isNegativeQuadrant) {
    sine *= -1;
  }

  return sine;
}

int sin16LUT(int index)
{
  return sinLUT[index];
}

/***********************************************************

	intSqrt

	fast integer sqrt

***********************************************************/
unsigned long intSqrt(unsigned long val)
{
  unsigned long mulMask = 0x0008000;
  unsigned long retVal = 0;

  if (val > 0) {
    while (mulMask != 0) {
      retVal |= mulMask;
      if ((retVal * retVal) > val) {
        retVal &= ~mulMask;
      }

      mulMask >>= 1;
    }
  }

  return retVal;
}

// ************************************************************************************************************
//  *  *   *   *  *  COMMON FUNCTIONS  *   *   *   *   *
// ************************************************************************************************************

/****************************************************************************
   Write all information buffered in the EEPROM to the SD card as CSV
 ****************************************************************************/

void flushEEpromBuffer(void) {

  //digitalWrite(RED_PIN, HIGH);
  // Sd Data writes often take between 15 and 25 milliseconds

  Vcc2 = readInternalVcc();  // and keep an eye on our voltage regulator
  if (Vcc2 < 2900) {         // if internal VCC is low, dont write to the SD card!
#ifdef ECHO_TO_SERIAL
    Serial.println(F("VReg output too low!"));
#endif
    error();
  }

#ifdef ECHO_TO_SERIAL
  Serial.println(F("--Writing to SDcard --"));
  //Serial.print(F("Current eeprom page address:"));Serial.print(CurrentPageStartAddress );
  //Serial.print(F("-now set to zero  & Cycle:"));Serial.println(Cycle);
  Serial.flush();
#endif

  file.open(FileName, O_RDWR | O_AT_END); // open the file for write at end like the Native SD library

  //if (!file.open(FileName, O_RDWR | O_AT_END)) { //error reporting?
  //#ifdef ECHO_TO_SERIAL
  //Serial.println(F("Open file fail"));
  //#endif
  //error();
  //}

  CurrentPageStartAddress = 0; //reset the page counter back to the beginning of the eeprom stack

  for (int i = 0; i < SamplesPerCycle; i++) {  //loop to read from I2C ee and write to SD card

    for (int j = 0; j < PagesBuffered2Eeprom; j++) {

      digitalWrite(RED_PIN, HIGH);  //Let the user know SD card writing is in progress
      Read_i2c_eeprom_page(EEPROM_ADDRESS, CurrentPageStartAddress, EEPROMBuffer, sizeof(EEPROMBuffer) );  //there will be a few blank spaces
      CurrentPageStartAddress += EEPromPageSize;
      digitalWrite(RED_PIN, LOW);

      file.write(EEPROMBuffer, sizeof(EEPROMBuffer));
      //BytesWrittentoSD = BytesWrittentoSD + sizeof(EEPROMBuffer);
    } // terminator: for (int j = 0; j < PagesBuffered2Eeprom; j++)

    file.println();  //add a carridge return to the file
    //BytesWrittentoSD = BytesWrittentoSD + 2;  //plus 2 for the space and c return
    countLogs++;

    // What follows is part of the accounting I used to do to make sure that file syncs occured before I hit the 512byte limit of the SD buffer
    // BUT this sync stuff might not be necessary any more? see:  http://forum.arduino.cc/index.php?topic=281487.0

    // The documentation says any application which writes to a file using print(), println() or write() must call sync()
    // You can reach a Max 512 bytes before you must sync to force data and directory information to be written to the SD Card.
    // Each cycle writes (32bytes * number of pages) + 2bytes for CR & termination character
    // so you have to sync BEFORE your next cycle IF that next cycle would put you over the 512 byte limit
    // 2PAGE cycles should can trigger at 462-1 (7x66 characters-462), 3 page cycles should be set to trigger  just below 490 (5x3pagecycles@98characters per)
    // 4pg cycle triggers below 454 // 5pges below 484 (486=3 cycles of 5)  //  6pages trigger below 389 bytes
    // 6 pg (130 bytes per cycle) 3*130=390, 4*130=520=0ver...Only able to write three 6-buffer cycles before sync...this is going to use alot more power...

    //if(BytesWrittentoSD > 440) {
    // syncTheFile;  //A sync costs 2048 bytes of IO
    // BytesWrittentoSD=0; }

  }  // terminator:    for(int i = 0; i < SamplesPerCycle; i++) loop

  if (DayEventBuffer[0] != '\0') { //no point in printing it if the array is still empty  http://stackoverflow.com/questions/1793867/best-way-to-check-if-a-character-array-is-empty
    file.write(DayEventBuffer, sizeof(DayEventBuffer)); //28 characters, loaded once per day at midnight when alarm hour rolls over
    file.println();//carridge return
  }

  file.close();
  CurrentPageStartAddress = 0; //now that we have dumped the eeprom to the sd, we can start filling it from the beginning again

  //Run the SdFat bench example.  it will print the max latency.  http://forum.arduino.cc/index.php/topic,109862.0.html
  //A latency of over 300 ms is unusually long.  150-200 ms is more common.

  PSvolts = readExternalPSvolts();  // Check how far the PS fell after the load of writing to the SD card
  //Note: reading this here will look like noise on our PS voltage data because the SD write process can pull the PS down by 40-50mv when the batteries get old

}  // terminator: END of function that flushs eeprom to SD card


/**********************************************
   SD card funtions : Create NEW file & SYNC
 ***********************************************/

// from http://forums.adafruit.com/viewtopic.php?f=31&t=17964

void createLogFile(void) {
  // create a new file, up to 100,000 files allowed
  // we will create a new file every time this routine is called
  // If we are creating another file after fileInterval, then we must
  // close the open file first.
  if (file.isOpen()) {
    file.close();
  }
  for (uint16_t i = 0; i < 100000; i++) {
    FileName[3] = i / 10000 + '0';
    FileName[4] = i / 1000  + '0';
    FileName[5] = i / 100   + '0';
    FileName[6] = i / 10    + '0';
    FileName[7] = i % 10    + '0';
    // O_CREAT - create the file if it does not exist
    // O_EXCL - fail if the file exists  O_WRITE - open for write
    if (file.open(FileName, O_CREAT | O_EXCL | O_WRITE)) break;
    //if you can open a file with the new name, break out of the loop
  }

  // clear the writeError flags generated when we broke the new name loop
  //file.writeError = 0; //this older code is not compatible with the new version of sdFat!

  if (!file.isOpen()) {
    Serial.println(F(" Disk Full Error"));
    error();
  }
  Serial.print(F("Logging to: "));
  Serial.println(FileName);

  // fetch the time   //is this what takes away 5 minutes from our clock?
  DateTime now = RTC.now();
  // set creation date time
  if (!file.timestamp(T_CREATE, now.year(), now.month(), now.day(), now.hour(),
                      now.minute(), now.second() )) {
    Serial.println(F("Can't timestamp the new file"));
    error();
  }
  // set write/modification date time
  if (!file.timestamp(T_WRITE, now.year(), now.month(), now.day(), now.hour(),
                      now.minute(), now.second() )) {
    Serial.println(F("Can't write to the new file"));
    error();
  }
  // set access date
  if (!file.timestamp(T_ACCESS, now.year(), now.month(), now.day(), now.hour(),
                      now.minute(), now.second() )) {
    Serial.println(F("Can't set new file access date"));
    error();
  }
  file.sync();

  //    if (!file.sync()) {
  // check if error writing
  //    Serial.println(F("File Syncing Error"));error();
  //  }

  writeHeaderInfo();

  file.close();

#ifdef ECHO_TO_SERIAL
  Serial.println(F("New log file created on the SD card!"));
  Serial.flush();
#endif  // ECHO_TO_SERIAL

  /* this old code is not compatible with new SD fat!
    // write out the header to the file, only upon creating a new file
    tempBool = bool(file.getWriteError());
    if (!tempBool) {   //older versions of sd fat worked with:  if (file.writeError) {
      // check if error writing
      #ifdef ECHO_TO_SERIAL
      Serial.println(F(" Can't write new file header"));Serial.flush();
      #endif
      error();
    }
  */

  //the New file creation event is one of the biggest power loads on the system, so I am using it here as a "test" of the strength of the batteries
  Vcc2 = readExternalPSvolts();  //check what the post new file creation power supply voltage is
  // readExternalPSvolts routine shuts down the system if voltage is too low for SD writing.

  file.open(FileName, O_RDWR | O_AT_END);
  file.print(F("PS(mV)@ New file:")); file.println(Vcc2); file.println();
  file.close();

}   //end of the create new log file routine

/**********************************************
   SD card funtions : Write HEADER INFORMATION
 ***********************************************/
void writeHeaderInfo(void)
{

  // HEADER INFO
  file.println(F("Cave Pearl Project: If found please email: patricia-at-earth.northwestern.edu"));
  file.println(F("Abby's Calibration Test"));
  file.print(F("Logger#,"));
  file.println(loggerNumber);
  file.print(F("Unit Notes:, "));
  file.println(unitNotes);

  file.print(F("Sensors:,"));

#ifdef unregulatedMCU
  file.print(F("TinyDuino,"));
#endif

#ifdef vRegulatedMCU
  file.print(F("3.3v reg'ed,"));
#endif

#ifdef TS_TMP102
  file.print(F("TMP102,"));
#endif

#ifdef TS_DS18B20
  file.print(F("DS18B20,"));
#endif

#ifdef MCP9808_I2CADDR
  file.print(F("MCP9808,"));
#endif

#ifdef BMA180_ADDRESS
  file.print(F("Bma180,"));
#endif

#ifdef BMA250_ADDRESS
  file.print(F("Bma250,"));
#endif

#ifdef HMC5883_ADDRESS
  file.print(F("HMC5883,"));
#endif

#ifdef LSM303_ADDRESS_MAG
  file.print(F("LSM303_MAG,"));
#endif

#ifdef MS5803_02_ISON
  file.print(F("MS5803_02,"));
#endif

#ifdef MS5805_02_ISON
  file.print(F("MS5805_02,"));
#endif

#ifdef MS5803_05_ISON
  file.print(F("MS5803_05,"));
#endif

#ifdef HTDU21D_ADDRESS
  file.print(F("HTDU21D,"));
#endif

#ifdef ADXL345_ISON
  file.print(F("ADXL345 (DRIP) ")); file.print(int(getRate() / 2)); file.print(F("Hz,")); file.print(F("Sensitivity:")); file.print(tapSensitivity);
#endif

  file.println();

#if defined(MS580X_I2C_ADDRESS) //If you have a pressure sensor on, write cal constants to the sd card

  file.println(F("Ms580X Calibr. constants:"));
  file.print(F("C1=SENS(t1):,"));
  file.println(sensorCoeffs[1]);
  file.print(F("C2=SENS(t1):,"));
  file.println(sensorCoeffs[2]);
  file.print(F("C3=TCS:,"));
  file.println(sensorCoeffs[3]);
  file.print(F("C4=TCO:,"));
  file.println(sensorCoeffs[4]);
  file.print(F("C5=RTreff:,"));
  file.println(sensorCoeffs[5]);
  file.print(F("C6=TEMPSENS:,"));
  file.println(sensorCoeffs[6]);
  file.println();

#endif

  file.print(F("The sample interval: "));
  file.print(SampleIntervalMinutes);
  file.print(F(" min. "));
#ifdef SampleIntSeconds
  file.print(F(" "));
  file.print(SampleIntSeconds);
  file.print(F(" Sec. "));
#endif
  file.println();

#if defined(BMA250_ADDRESS) || defined(BMA180_ADDRESS)//FLOW sensor configuration
  file.println(F("YY/MM/DD HH:MM,Bat(mV),(rawT)*.0625=(C),AccX,AccY,AccZ,Temp(C),CompX,CompY,CompZ,RAW tilt(Degrees*100),CoinCell(mV)"));
#endif
#if defined(LSM303_ADDRESS_ACCEL) //LM303 FLOW sensor configuration
  file.println(F("YY/MM/DD HH:MM,Bat(mV),(rawT)*.0625=(C),AccX,AccY,AccZ,RAW tilt(Degrees*100),CompX,CompY,CompZ,CoinCell(mV)"));
#endif
#ifdef MS580X_I2C_ADDRESS  //if we are pressure sensing, the unit is stationary, and we dont have the acc or comass running
  file.print(F("YY/MM/DD HH:MM,Bat(mV),(rawT)*.0625=(C),D1(Press)MS580x,D2(Temp)MS580x,(mbar)1stOrd,(C)1stOrd"));
#ifdef HTDU21D_ADDRESS
  file.print(F(",(rawTemp)HTDU21D,(rawRH)HTU21D,R.Humidity(%),RTC(°C)"));
#endif
#endif
#ifdef ADXL345_ISON
  file.println(F("YY/MM/DD HH:MM,Bat.(mV),CoinCell(mV),Drip Count,RTC(°C),AccX,AccY,AccZ,DayOfAccRead"));
#endif

  file.println();

}



/**********************************************
   SD card funtions : SYNC THE FILE
 ***********************************************/
// this might not be necessary any more?? http://forum.arduino.cc/index.php?topic=281487.0

void syncTheFile(void)
{
  /* don't sync too often - requires 2048 bytes of I/O to SD card. 512 bytes of I/O if using Fat16 library */

  digitalWrite(RED_PIN, HIGH);
  if (!file.sync()) {
#ifdef ECHO_TO_SERIAL
    Serial.println(F("File Sync error")); Serial.flush();
#endif
    error(); //terminal error if you cant save data to sd card
  }  // 15-20 ms for syncing
  digitalWrite(RED_PIN, LOW);

  PSvolts = readExternalPSvolts();  //Shuts down if sync brings the voltage too low

}

/*****************************************************************************************************************
   i2c EEPROM    READ & WRITE PAGEs
 *******************************************************************************************************************/
// see http://www.hobbytronics.co.uk/eeprom-page-write
// Address is a page address - But data can be maximum of 28 bytes
// The Arduino Wire library only has a 32 character buffer, so that is the maximun we can send using Arduino.
// This buffer includes the two address bytes which limits our data payload to 30 bytes, but I have never gotten this routine to
// work beyond 28 bytes of payload.
// The time overhead for page writes  = 5ms, whilst the overhead for writing individual bytes = 3.5ms
void Write_i2c_eeprom_page( int deviceaddress, unsigned int eeaddress, char* data)
{
  unsigned char i = 0;
  unsigned int  address;
  address = eeaddress;
  Wire.beginTransmission(deviceaddress);
  Wire.write((int)((address) >> 8));   // MSB
  Wire.write((int)((address) & 0xFF)); // LSB
  do {
    Wire.write((byte) data[i]);
    i++;
  }
  while (data[i]);
  Wire.endTransmission();
  //OR delay(6);  // data sheet says 5ms for page write  (+ ATmega328 run ~3mA at 3.3volts = about the same as the eeprom! but only0.36mA in pwer_down)
  LowPower.powerDown(SLEEP_15MS, ADC_OFF, BOD_OFF);
}

void Read_i2c_eeprom_page( int deviceaddress, unsigned int eeaddress, char* data, unsigned int num_chars)
{
  unsigned char i = 0;
  Wire.beginTransmission(deviceaddress);
  Wire.write((int)(eeaddress >> 8));   // MSB
  Wire.write((int)(eeaddress & 0xFF)); // LSB
  Wire.endTransmission();
  Wire.requestFrom(deviceaddress, (num_chars - 1));
  while (Wire.available()) data[i++] = Wire.read();
}

/**********************************************
   SLEEP and wait for RTC
 ***********************************************/
//code that is commented out is from before the adoption of the Rocket Scream sleep library
//just keeping it here in case I decide I want to switch back to the old method of sleeping

void sleepNwait4RTC()
{

#ifdef RTCPOWER_PIN  //if using pin power on RTC, depower it now:
  pinMode (RTCPOWER_PIN, INPUT);
  digitalWrite(RTCPOWER_PIN, LOW); // RTC vcc connected to this pin - driving this LOW FORCES to the RTC to draw power from the coin cell during sleep
#endif

  noInterrupts ();          //  same as cli() make sure we don't get interrupted before we sleep
  attachInterrupt(0, clockTrigger, LOW);
  interrupts ();            // same as sei() interrupts allowed now, next instruction WILL be executed
  LowPower.powerDown(SLEEP_FOREVER, ADC_OFF, BOD_OFF);
  //HERE AFTER WAKING UP
  detachInterrupt(0);

#ifdef RTCPOWER_PIN
  digitalWrite(RTCPOWER_PIN, HIGH); // about to generate I2C traffic, so power the rtc from the pin
  pinMode (RTCPOWER_PIN, OUTPUT);
#endif

}
/**********************************************
   CLOCK TRIGGER FLAG
 ***********************************************/
void clockTrigger() {
  clockInterrupt = true; //do something quick, flip a flag, and handle in loop();
}
/**********************************************
   CLEAR CLOCK TRIGGER
 ***********************************************/
// clear alarm interupt from http://forum.arduino.cc/index.php?topic=109062.0

void clearClockTrigger()
{
  i2c_writeRegBits(DS3231_ADDRESS, DS3231_STATUS_REG, 0, Bit0_MASK); //Bit 0 = Alarm 1 Flag (A1F)
  i2c_writeRegBits(DS3231_ADDRESS, DS3231_STATUS_REG, 0, Bit1_MASK); //Bit 1 = Alarm 2 Flag (A2F) on DS3231 - see pg17 of datasheet
  clockInterrupt = false;         //Finally clear the flag we use to indicate the trigger occurred
}

/* To set Duration of Rocket Screams low power sleep mode. Use SLEEP_FOREVER to use other wake up resource:
  (a) SLEEP_15Ms - 15 ms sleep  (THERE IS A TYPO IN THE LIBRARY - SLEEP_MS defined as SLEEP_Ms (lower case s)
  (b) SLEEP_30MS - 30 ms sleep, SLEEP_60MS - 60 ms sleep, SLEEP_120MS - 120 ms sleep, SLEEP_250MS - 250 ms sleep, SLEEP_500MS - 500 ms sleep
  (g) SLEEP_1S - 1 s sleep, SLEEP_2S - 2 s sleep, SLEEP_4S - 4 s sleep, SLEEP_8S - 8 s sleep  //this did not work for me?  perhaps another typo?
  (k) SLEEP_FOREVER - Sleep without waking up through WDT
  example: LowPower.powerDown(SLEEP_15Ms, ADC_OFF, BOD_OFF); */

/*******************************************************
   READ PSvolts          using internal 1.1 v  OR analog pin
 ********************************************************/
// from http://forum.arduino.cc/index.php/topic,15629.0.html and http://forum.arduino.cc/index.php?topic=88935.0

int readExternalPSvolts()    //the supply voltage via analog read from a 4.7M ohm resistor divider
{
  int result;
  //10 bit resolution on arduinos internal ADC, returning integers from 0 to 1023
  // we have a simple equal value resistor divider supplying this pin so it can read Vraw above PSvolts

#ifdef vRegulatedMCU  //ProMini Clones & Rocket Ultra boards
  int avrgVraw = 0;
  int ActualRaw_mV = 0;
  int vRaw = 0;
  analogRead(A0);        //first read no good as pin cap not charged
  delay(1);              //pin settling time
  vRaw = analogRead(A0);
  avrgVraw = avrgVraw + vRaw;
  delay(1);
  vRaw = analogRead(A0);
  avrgVraw = avrgVraw + vRaw;
  avrgVraw = avrgVraw / 2; //avrg of readings
  float volts = (avrgVraw / resistorFactor) * referenceVolts ; // calculate the ratio  Need to make sure reference volts is accurate by measurement on each unit!
  result = (volts * 1000); // conv to millivolts
#endif

#ifdef unregulatedMCU  //Tinyduinos do not have a voltage divider, and use the 1.1vref trick
  result = readInternalVcc();
#endif

#ifdef ECHO_TO_SERIAL  //this allows to write SD data when connected to the USB cable (so the pin read on readexternalvolts yeilds zero!
  result = result + 5000; //but if you start seeing > 7V and there is no smoke rising form the board you know you forgot something
#endif

  // this changes the color of the read sensor LED pip to warn that the battery voltage is getting low
  if (result < BlueVWarning) {
    READSENSOR_PIN = BLUE_PIN;  //if sensor pip is blue, batteries are getting low
  }
  if (result < RedVWarning) {
    READSENSOR_PIN = RED_PIN;  //If the sensor pip goes red, it is time to change the batteries.
  }
  if (result < CutoffVoltage) { //The pro mini board vregulators need 3.35 v minimum for stable output
    if (file.isOpen()) {
      file.close();
    }
#ifdef ECHO_TO_SERIAL
    Serial.println(F("PS too low for SD card writing!"));
#endif
    error();
  }

  return result;
}

// Need to switch between external analog pin read (above) to vcc trick (below) depending on which mcu board is used

int readInternalVcc() //use this one on systems without a voltage divider - Like the TinyDuino
{
  int result;
  ADMUX = _BV(REFS0) | _BV(MUX3) | _BV(MUX2) | _BV(MUX1);
  delay(2); // Wait for Vref to settle
  ADCSRA |= _BV(ADSC); // Convert
  while (bit_is_set(ADCSRA, ADSC));
  result = ADCL;
  result |= ADCH << 8;
  result = 1126400L / result; // Back-calculate AVcc in mV

#ifdef ECHO_TO_SERIAL
  readCoinCell();
#endif

  return result;
  // The spec sheet gives a nominal value of 1.1 volts, but states that it can vary from 1.0 to 1.2 volts.
  // That means that any measurement with this method could be off by as much as 10%
  // see http://provideyourown.com/2012/secret-arduino-voltmeter-measure-battery-voltage/
  // about replacing the constant 1125300L with    scale_constant = internal1.1Ref * 1023 * 1000
  // where internal1.1Ref = 1.1 * [Vcc1 (per actual voltmeter) / Vcc2 (per this readInternalVcc() function)]
}

int readCoinCell(void)
{
  int avrgVraw = 0;
  int ActualRaw_mV = 0;
  int vRaw = 0;
  vRaw = analogRead(A1); //throw away this reading, cap is not charged yet
  delay(1);
  vRaw = analogRead(A1);
  avrgVraw = avrgVraw + vRaw;
  delay(1);
  vRaw = analogRead(A1);
  avrgVraw = avrgVraw + vRaw;
  avrgVraw = avrgVraw / 2; //avrg of readings
  CoinCellV = (avrgVraw * (3.3 / 1023.0)) * 2000; //using a 2x 4.7 Meg ohm divider so the actual voltage is 2X the number here
  //CoinCellV = (vRaw*(3.35/1023.0))*1000;
}

// ************************************************************************************************************
// I2C   REGISTER   FUNCTIONS
// ************************************************************************************************************

/* Writes newValue to address register bits on device using the bitmask to protect the other bits */

byte i2c_writeRegBits(int DEVICE, byte address, byte newValue, byte mask) {
  byte result, current;
  current = i2c_readRegByte(DEVICE, address);  //load the existing register contents
  result = i2c_writeRegByte(DEVICE, address, (current & ~mask) | (newValue & mask));  //eliminate old bits, and OR in the new ones
  return result;
}

// based on https://github.com/makerbot/BMA180-Datalogger/blob/master/bma180-datalogger-shield/bma180-logger/bma180.ino

byte i2c_readRegByte(int dev_address, byte reg_address)  //MUST be interger for the i2c address
{
  byte temp;

  Wire.beginTransmission(dev_address); //set destination target
  Wire.write(reg_address);
  Wire.endTransmission();

  Wire.beginTransmission(dev_address); //get data from device
  Wire.requestFrom(dev_address, 1);
  while (Wire.available())
  {
    temp = Wire.read();
  }
  return temp;
}


byte i2c_writeRegByte(int dev_address, byte reg_address, byte data)
{
  byte result;

  Wire.beginTransmission(dev_address);
  Wire.write(reg_address);
  Wire.write(data);
  result = Wire.endTransmission();

  //should I do some error checking??
  if (result > 0)
  {

#ifdef ECHO_TO_SERIAL   //only call halt on error if in debug mode
    Serial.print(F("FAIL in I2C reg write! Result code is "));
    Serial.println(result);
    error();
#endif
  }

  LowPower.powerDown(SLEEP_15MS, ADC_OFF, BOD_OFF); // some of my sensors need settling time after a register change.

  return result;
}

bool getRegisterBit(int devAddress, byte regAdress, int bitPos) {
  byte bytebuffer1;
  readFrom(devAddress, regAdress, 1, &bytebuffer1);
  return ((bytebuffer1 >> bitPos) & 1); //& 1 gets rid of the other bits
}

byte setRegisterBit(int devAddress, byte regAddress, int bitPos, bool state) {
  byte result;
  byte bytebuffer1;
  readFrom(devAddress, regAddress, 1, &bytebuffer1);
  if (state) {
    bytebuffer1 |= (1 << bitPos);  // forces nth bit of bytebuffer1 to be 1.  all other bits left alone.
  }
  else {
    bytebuffer1 &= ~(1 << bitPos); // forces nth bit of bytebuffer to be 0.  all other bits left alone.
  }
  result = writeTo(devAddress, regAddress, bytebuffer1);
  return result;
}

// Writes val to address register on device  //SAME AS i2c_writeRegByte - COULD REMOVE THIS
byte writeTo(int DEVICE, byte address, byte val) {
  byte result;
  Wire.beginTransmission(DEVICE); // start transmission to device
  Wire.write(address);             // send register address
  Wire.write(val);                 // send value to write
  result = Wire.endTransmission();         // end transmission
  return result;
}

// Reads num bytes starting from address register on device in to _buff array  THIS ROUTINE COULD REPLACE I2C_READREGBYTE?
void readFrom(int DEVICE, byte address, int num, byte _buff[]) {
  Wire.beginTransmission(DEVICE); // start transmission to device
  Wire.write(address);             // sends address to read from
  Wire.endTransmission();         // end transmission

  Wire.beginTransmission(DEVICE); // start transmission to device
  Wire.requestFrom(DEVICE, num);    // request bytes from device

  int i = 0;
  while (Wire.available())        // device may send less than requested (abnormal)
  {
    _buff[i] = Wire.read();    // receive a byte
    i++;
  }
  if (i != num) {
    //status = ERROR;
    //error_code = READ_ERROR;
#ifdef ECHO_TO_SERIAL   //only call halt on error if in debug mode
    error();
#endif
  }
  Wire.endTransmission();         // end transmission
}


/**********************************************
   BOILERPLATE
 ***********************************************/
void serial_boilerplate()
{
  Serial.println(F("The Cave Pearl: An Open Source Data Logger for Hydrological Research"));
  Serial.println();
  Serial.println(F("Developed by Edward Mallon:    http://edwardmallon.wordpress.com/"));
  Serial.println();
  Serial.flush();
}

/**********************************************
   FREE RAM AVAILIABLE
 ***********************************************/
// from: http://learn.adafruit.com/memories-of-an-arduino/measuring-free-memory

int freeRam ()
{
  extern int __heap_start, *__brkval;
  int v;
  return (int) &v - (__brkval == 0 ? (int) &__heap_start : (int) __brkval);
}


/**********************************************
   ERROR HANDLER "Houston we have a problem..."
 ***********************************************/
// more advanced debugging: http://forum.arduino.cc/index.php?topic=203282.0
void error()
{
  if (file.isOpen()) {
    file.close();
  }

#ifdef ECHO_TO_SERIAL
  Serial.flush();
#endif

  digitalWrite(RED_PIN, LOW);
  digitalWrite(GREEN_PIN, LOW);
  digitalWrite(BLUE_PIN, LOW);

  for (int CNTR = 0; CNTR < 240; CNTR++) { //# of seconds of flashing red light on error = CNTR/2 with 250 ms delay
    digitalWrite(RED_PIN, HIGH);
    LowPower.powerDown(SLEEP_250MS, ADC_OFF, BOD_ON);
    digitalWrite(RED_PIN, LOW);
    LowPower.powerDown(SLEEP_250MS, ADC_OFF, BOD_ON);
  }

  //}
#if defined RTCPOWER_PIN
  pinMode(RTCPOWER_PIN, INPUT);    //stop sourcing or sinking current
  digitalWrite(RTCPOWER_PIN, LOW); // driving this LOW FORCES to the RTC to draw power from the coin cell
#endif
#if defined POWERDOWN_PIN
  digitalWrite(POWERDOWN_PIN, HIGH);// driving this pin high shuts down the system if pololu power switch attached
  delay(10);// just in case it takes time to power down...
#endif

  LowPower.powerDown(SLEEP_FOREVER, ADC_OFF, BOD_ON); //if pololu shutdown does not happen go to sleep, leave BODon
  // or use:  sleepNwait4WDT(); //if you go to sleep without FIRST setting the wdt, this is sleep forever!

}

// ************************************************************************************************************
//   *  *   *  *  *  *  *  *  *  *  SENSOR FUNCTIONS  *  *  *  *  *  *  *  *  *  *  *  *  *
// ************************************************************************************************************

// ************************************************************************************************************
// DS18B20  ONE WIRE TEMPERATURE
// ************************************************************************************************************
// this returns the temperature from one DS18S20 using 12 bit conversion
// also see Dallas Temperature Control library by Miles Burton: http://milesburton.com/Dallas_Temperature_Control_Library

#if defined(TS_DS18B20)

int readDS18B20Temp()
{
  //byte data[12];
  byte data[2];
  ds.reset();
  ds.select(addr);
  ds.write(0x44); // start conversion, read temperature and store it in the scratchpad
  /*
    //this next bit creates a 1 second WDT delay for the DS18b20 temp conversion
    //The time needed between the CONVERT_T command and the READ_SCRATCHPAD command has to be at least
    //750 millisecs (can be shorter if using a D18B20 type with resolutions < 12 bits)
    //if you start getting "85" all the time you did not wait long enough
    // power saving during sleep from http://www.gammon.com.au/forum/?id=11497
    MCUSR = 0;   // clear various "reset" flags
    WDTCSR = bit (WDCE) | bit (WDE); // allow changes, disable reset
    // set interrupt mode and an interval
    WDTCSR = bit (WDIE) | bit (WDP2) | bit (WDP1);    //this creates a 1 sec wdt delay for the temp conversion
    wdt_reset();  // pat the dog
    set_sleep_mode (SLEEP_MODE_PWR_DOWN);
    sleep_enable();
    sleep_cpu ();
    // resume here after wdt interrupt happens - cancel sleep as a precaution
    sleep_disable();
    delay(3);
  */
  LowPower.powerDown(SLEEP_1S, ADC_OFF, BOD_OFF);

  byte present = ds.reset();
  ds.select(addr);
  ds.write(0xBE); // Read Scratchpad
  for (int i = 0; i < 2; i++)
  { // we read 9 bytes? but you only use two of them?
    data[i] = ds.read();
  }
  byte MSB = data[1];
  byte LSB = data[0];
  int tempRaw = ((MSB << 8) | LSB); //using two's compliment
  //TEMP_degC = tempRaw / 16;
  return tempRaw;
}

#endif


// ************************************************************************************************************
// Sample SMOOTHING FUNCTION (used by Accelerometers & TMP102)
// ************************************************************************************************************
// this smoothing function based on Paul Badger's  http://playground.arduino.cc/Main/DigitalSmooth
// "int *inputArray" passes an array to the function - the asterisk indicates the array name is a pointer

#if defined(filterSamples)
int digitalSmooth(int *inputArray) {
  int j, k, temp, top, bottom;
  long total;
  static int i;
  boolean done;

  done = 0;                // flag to know when we're done sorting
  while (done != 1) {      // simple swap sort, sorts numbers from lowest to highest
    done = 1;
    for (j = 0; j < (filterSamples - 1); j++) {
      if (inputArray[j] > inputArray[j + 1]) {    // numbers are out of order - swap
        temp = inputArray[j + 1];
        inputArray [j + 1] =  inputArray[j] ;
        inputArray [j] = temp;
        done = 0;
      }
    }
  }

#ifdef ECHO_TO_SERIAL
#ifdef TS_TMP102
  Serial.print(F("TMP102:"));
#endif
#ifdef BMA180_ADDRESS
  Serial.print(F("BMA180:"));
#endif
#ifdef BMA250_ADDRESS
  Serial.print(F("BMA250:"));
#endif
  Serial.println(F("Sorted Raw readings:"));
  for (j = 0; j < (filterSamples); j++) {   // print the array for debugging
    Serial.print(inputArray[j]);
    Serial.print(F("   "));
  }
  Serial.println(); Serial.flush();
#endif

  // throw out top and bottom 15% of samples - limit to throw out at least one from top and bottom
  bottom = max(((filterSamples * 15)  / 100), 1);
  top = min((((filterSamples * 85) / 100) + 1  ), (filterSamples - 1));   // the + 1 is to make up for asymmetry caused by integer rounding
  k = 0;
  total = 0;
  for ( j = bottom; j < top; j++) {
    total += inputArray[j];  // total remaining indices
    k++;
    // Serial.print(sensSmoothArray[j]);Serial.print("   "); //more debugging
  }
  //  Serial.println(); Serial.print("average = "); Serial.println(total/k); //more debugging
  return total / k;  // divide by number of samples

}

#endif


// ************************************************************************************************************
// RTC temperature function
// ************************************************************************************************************
// also see https://github.com/mizraith/RTClib  or https://github.com/akafugu/ds_rtc_lib for more DS3231 specific libs
// could also use RTC.getTemperature() from the library here as in:
// RTC.convertTemperature();             //convert current temperature into registers
// Serial.print(RTC.getTemperature());   //read registers and display the temperature
// get temp from  http://forum.arduino.cc/index.php/topic,22301.0.html


float readRTCtemp()
{
  float temp3231;
  //temp registers (11h-12h) get updated automatically every 64s
  Wire.beginTransmission(DS3231_ADDRESS);
  Wire.write(0x11);
  Wire.endTransmission();
  Wire.requestFrom(DS3231_ADDRESS, 2);

  if (Wire.available()) {
    bytebuffer1 = Wire.read(); //2's complement int portion
    bytebuffer2 = Wire.read(); //fraction portion
    temp3231 = ((((short)bytebuffer1 << 8) | (short)bytebuffer2) >> 6) / 4.0; // Allows for readings below freezing - Thanks to Coding Badly
    //temp3231 = (temp3231 * 1.8) + 32.0; // to Convert Celcius to Fahrenheit
  }
  else {
    temp3231 = 0.0; //got no data from RTC
  }
  if (bytebuffer1 & 0b10000000) //check if -ve number  - this might be redundant with the above code?
  {
    temp3231 = temp3231 * -1.0;
  }

  return temp3231;
}









