# reflowOvenController

## Key Features

- **Assembly Language Firmware:** The microcontroller firmware is programmed in assembly language for precise control.

- **Temperature Measurement:** Oven temperature is measured between 25°C and 240°C using a K-type thermocouple with cold junction compensation.

- **Oven Control:** A 1500W toaster oven is regulated via a solid-state relay (SSR) using PWM for power delivery.

- **User Interface:**

    Pushbuttons allow profile parameter selection.

    An LCD displays current temperature, elapsed time, and process state.

    Start/Stop buttons provide process control.

- **Real-Time Plotting:** Temperature readings are sent via serial port to a PC for live plotting with color-coded stages.

- **Process Summary:** Displays maximum temperature, slopes, averages, and time spent in each stage.

- **Multimeter Validation:** Simultaneously reads and compares voltage from a second serial port, converting it to temperature.

- **Data Export & Email:** Automatically saves data to temperature_data.xlsx and emails the file if an address is provided.

- **Audio Feedback:** Plays background music during the final stage using pygame.mixer.

- **Safety Shutdown:** The system automatically shuts down if the oven fails to reach 50°C within the first 60 seconds.
