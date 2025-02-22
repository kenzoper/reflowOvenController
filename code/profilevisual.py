#!/usr/bin/env python3
"""
Combined Temperature Monitoring and Validation with Tkinter + Matplotlib,
featuring color-coded states, slope/average computations, summary on state 5,
Excel export, and email sending at program exit.
"""

import tkinter as tk
from tkinter import Label, Entry, Button, StringVar, Frame
import serial
import time
import numpy as np
from collections import deque

import matplotlib
matplotlib.use("TkAgg")
from matplotlib.figure import Figure
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.animation as animation
from matplotlib.collections import LineCollection

import pygame        # For playing audio during state 5
import kconvert      # For converting mV -> temperature (K-type thermocouple)

from openpyxl import Workbook       # For Excel export
import smtplib                      # For sending email
from email.message import EmailMessage

# -------------- GMAIL CONFIG --------------
SMTP_SERVER   = 'smtp.gmail.com'
SMTP_PORT     = 465          # TLS/SSL port
SMTP_USERNAME = 'xxx@gmail.com'
SMTP_PASSWORD = 'xxx'
FROM_EMAIL    = 'xxx'
EMAIL_SUBJECT = 'Temperature Data Report'
EMAIL_BODY    = 'Attached is the temperature data report from the monitoring session.'

# -------------- AUDIO CONFIG --------------
AUDIO_FILE = "song.mp3"  # Replace with the correct filename or path

# -------------- SERIAL PORTS --------------
MICROCONTROLLER_PORT = "COM5"  # Adjust as needed
MULTIMETER_PORT      = "COM12" # Adjust as needed

# -------------- TIME WINDOW FOR PLOTTING --------------
TIME_WINDOW = 900  # 900 seconds = 15 minutes

class CombinedApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Combined Temperature Monitor & Validation")
        self.protocol("WM_DELETE_WINDOW", self.on_close)  # On window close

        # ---------- MISC STATE VARIABLES ----------
        self.printed_header = False  # For printing a one-time header in the console
        self.data_rows = []         # For storing (micro_temp, state, multi_temp, error) to export later
        self.start_time = time.time()
        self.max_temp = float('-inf')  # Track overall max temperature from microcontroller
        self.is_playing = False        # For audio playback control in state 5

        # ---------- TRACKING STATE & TIME ----------
        # We’ll store (time, temp, state) in main deques for plotting
        self.time_data  = deque()
        self.temp_data  = deque()
        self.state_data = deque()

        # For slope/average calculations:
        self.state_time_data  = {1: deque(), 2: deque(), 3: deque(), 4: deque()}
        self.state_temp_data  = {1: deque(), 2: deque(), 3: deque(), 4: deque()}
        self.state_summaries  = {1: None,  2: None,  3: None,  4: None}
        self.saved_summary     = ""   # Holds the summary text displayed in state 5
        self.state_durations   = {}   # Track total time in each state
        self.last_state        = None
        self.last_state_start_time = None

        # ---------- INITIALIZE SERIAL PORTS ----------
        self.ser_micro = None
        try:
            self.ser_micro = serial.Serial(MICROCONTROLLER_PORT, 115200, timeout=1)
            print(f"Opened microcontroller port {MICROCONTROLLER_PORT} at 115200 baud.")
        except Exception as e:
            print(f"Error opening {MICROCONTROLLER_PORT}:", e)

        self.ser_multimeter = None
        try:
            self.ser_multimeter = serial.Serial(MULTIMETER_PORT, 9600, timeout=0.5)
            print(f"Opened multimeter port {MULTIMETER_PORT} at 9600 baud.")
        except Exception as e:
            print(f"Error opening multimeter port {MULTIMETER_PORT}:", e)

        # ---------- PYGAME AUDIO SETUP ----------
        pygame.mixer.init()
        try:
            pygame.mixer.music.load(AUDIO_FILE)
        except Exception as e:
            print(f"Could not load audio file '{AUDIO_FILE}':", e)

        # ---------- BUILD UI: LEFT = PLOT, RIGHT = MULTIMETER + EMAIL ----------
        self.plot_frame = Frame(self)
        self.plot_frame.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        self.val_frame = Frame(self)
        self.val_frame.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True, padx=10, pady=10)

        # ---------- SET UP MATPLOTLIB FIGURE & AXES ----------
        self.fig = Figure(figsize=(5, 4), dpi=100)
        self.ax  = self.fig.add_subplot(111)
        self.ax.set_xlabel("Time (s)")
        self.ax.set_ylabel("Temperature (°C)")
        self.ax.set_ylim(0, 260)

        # We'll use a LineCollection to display color-coded segments for each state
        self.segments = []
        self.segment_colors = []
        self.line_collection = LineCollection([], linewidth=2)
        self.ax.add_collection(self.line_collection)

        # We also want text on the plot for displaying info:
        self.info_text = self.ax.text(
            0.02, 0.95, "", transform=self.ax.transAxes,
            fontsize=12, verticalalignment='top'
        )
        self.state_text = self.ax.text(
            0.98, 0.95, "State: ?", transform=self.ax.transAxes,
            fontsize=14, verticalalignment='top',
            horizontalalignment='right', color="blue"
        )

        # Embed the figure in Tkinter
        self.canvas = FigureCanvasTkAgg(self.fig, master=self.plot_frame)
        self.canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)

        # Create animation
        self.ani = animation.FuncAnimation(self.fig, self.update_plot, interval=100, blit=False)

        # ---------- BUILD THE RIGHT-SIDE VALIDATION UI ----------
        self.build_validation_ui()

        # If we have a multimeter port open, start polling it
        if self.ser_multimeter:
            self.after(1000, self.update_validation)
        else:
            self.port_status_var.set("Multimeter port not available")

    # -------------------- UI FOR VALIDATION --------------------
    def build_validation_ui(self):
        Label(self.val_frame, text="Cold Junction Temp:").pack(pady=5)
        self.cj_temp_var = StringVar(value="22")
        Entry(self.val_frame, textvariable=self.cj_temp_var, width=7).pack(pady=5)

        Label(self.val_frame, text="Multimeter Reading:").pack(pady=5)
        self.dmm_out_var = StringVar(value="NO DATA")
        Label(self.val_frame, textvariable=self.dmm_out_var,
              font=("Helvetica", 20), fg="red").pack(pady=5)

        Label(self.val_frame, text="Thermocouple Temp:").pack(pady=5)
        self.temp_var = StringVar(value="----")
        Label(self.val_frame, textvariable=self.temp_var,
              font=("Helvetica", 40), fg="blue").pack(pady=5)

        self.port_status_var = StringVar(value="Multimeter status unknown")
        Label(self.val_frame, textvariable=self.port_status_var,
              font=("Helvetica", 12)).pack(pady=5)

        self.dmm_name_var = StringVar(value="--------")
        Label(self.val_frame, textvariable=self.dmm_name_var,
              font=("Helvetica", 12)).pack(pady=5)

        Label(self.val_frame, text="Recipient Email:").pack(pady=5)
        self.recipient_email_var = StringVar(value="")
        Entry(self.val_frame, textvariable=self.recipient_email_var, width=30).pack(pady=5)

        Button(self.val_frame, text="Exit", command=self.on_close).pack(pady=10)

    # -------------------- SLOPE CALCULATION --------------------
    def compute_slope(self, times, temps):
        if len(times) < 2:
            return 0.0
        times_arr = np.array(times)
        temps_arr = np.array(temps)
        slope, _ = np.polyfit(times_arr, temps_arr, 1)
        return slope

    # -------------------- MAIN PLOT UPDATE --------------------
    def update_plot(self, _frame):
        """
        Poll the microcontroller (COM5) for data in the format "0000.0000-x"
        Then update:
          • Color-coded line segments for each state
          • Time/data buffers
          • Slope/average tracking for states 1–4
          • Summary display in state 5
          • Music playback for state 5
        """
        if self.ser_micro and self.ser_micro.in_waiting > 0:
            try:
                # Read one line and parse
                line_data = self.ser_micro.readline().decode("utf-8").strip()
                if not line_data:
                    return self.line_collection,

                # Expect something like "0023.4500-1"
                if "-" in line_data:
                    temp_str, state_str = line_data.split("-")
                else:
                    # Fallback if no dash found
                    temp_str, state_str = line_data, "0"

                temp = float(temp_str)
                state = int(state_str)
                current_time = time.time() - self.start_time

                # Possibly start or stop music
                if state == 5 and not self.is_playing:
                    pygame.mixer.music.play(-1)  # loop indefinitely
                    self.is_playing = True
                elif state != 5 and self.is_playing:
                    pygame.mixer.music.stop()
                    self.is_playing = False

                # Track max temperature
                self.max_temp = max(self.max_temp, temp)

                # Store data in main buffers
                self.time_data.append(current_time)
                self.temp_data.append(temp)
                self.state_data.append(state)

                # For slope/average: store data in state-specific buffers if we have 1–4
                if state in self.state_time_data:
                    self.state_time_data[state].append(current_time)
                    self.state_temp_data[state].append(temp)

                # Remove old data from the main buffers
                while self.time_data and (self.time_data[0] < current_time - TIME_WINDOW):
                    self.time_data.popleft()
                    self.temp_data.popleft()
                    old_state_val = self.state_data.popleft()
                    if self.segments:
                        self.segments.pop(0)
                    if self.segment_colors:
                        self.segment_colors.pop(0)

                # Remove old data from the state-specific buffers
                for s in self.state_time_data:
                    while (self.state_time_data[s] and
                           self.state_time_data[s][0] < current_time - TIME_WINDOW):
                        self.state_time_data[s].popleft()
                        self.state_temp_data[s].popleft()

                # Compute slope or average for states 1–4
                for s in [1, 3]:
                    if len(self.state_time_data[s]) > 1:
                        slope_val = self.compute_slope(self.state_time_data[s],
                                                       self.state_temp_data[s])
                        self.state_summaries[s] = f"Slope {s}: {slope_val:.2f} C/s"
                for s in [2, 4]:
                    if len(self.state_temp_data[s]) > 0:
                        avg_val = np.mean(self.state_temp_data[s])
                        self.state_summaries[s] = f"Avg {s}: {avg_val:.2f} C"

                # Track time spent in each state
                old_state = self.last_state
                if self.last_state_start_time is None:
                    self.last_state_start_time = current_time
                    self.last_state = state
                elif state != self.last_state:
                    # End the old state's timer
                    duration = current_time - self.last_state_start_time
                    self.state_durations[self.last_state] = \
                        self.state_durations.get(self.last_state, 0) + duration
                    # Switch
                    self.last_state = state
                    self.last_state_start_time = current_time

                # Update segments for color-coded line
                if len(self.time_data) > 1:
                    # The newest segment is from the penultimate to the last point
                    segment = [
                        (self.time_data[-2], self.temp_data[-2]),
                        (self.time_data[-1], self.temp_data[-1])
                    ]
                    self.segments.append(segment)

                    # Decide color
                    # We change color on a state change, else keep the last color
                    color_map = {1:"blue", 2:"green", 3:"red", 4:"purple", 5:"orange",
                                 6:"brown", 7:"pink"}
                    if old_state is None or state != old_state:
                        color = color_map.get(state, "black")
                    else:
                        color = self.segment_colors[-1] if self.segment_colors else "blue"

                    self.segment_colors.append(color)

                    self.line_collection.set_segments(self.segments)
                    self.line_collection.set_colors(self.segment_colors)

                    # Adjust x-limits
                    self.ax.set_xlim(max(0, current_time - TIME_WINDOW), current_time)

                # Handle the summary display logic:
                # If we are in state 5, compile slope/avg from states 1-4, plus max temp + time
                if state == 5:
                    parts = []
                    for val in self.state_summaries.values():
                        if val:
                            parts.append(val)
                    # Add max temp
                    parts.append(f"Max Temp: {self.max_temp:.2f} C")
                    # Time per state
                    all_states = set(self.state_durations.keys())
                    all_states.add(state)
                    time_summary = ""
                    for s in sorted(all_states):
                        duration_s = self.state_durations.get(s, 0)
                        # If still in that state, add ongoing time
                        if s == state:
                            duration_s += current_time - self.last_state_start_time
                        time_summary += f"State {s}: {duration_s:.2f} s\n"
                    parts.append("Time Spent in Each State:\n" + time_summary)
                    self.saved_summary = "\n".join(parts)
                elif state == 1:
                    # Reset summary when entering state 1
                    self.saved_summary = ""

                # Display text on plot
                live_display = self.state_summaries.get(state, "")
                self.info_text.set_text(f"{live_display}\n{self.saved_summary}")

                # Update state text color
                color_map = {1:"blue", 2:"green", 3:"red", 4:"purple", 5:"orange",
                             6:"brown", 7:"pink"}
                st_color = color_map.get(state, "black")
                self.state_text.set_text(f"State: {state}")
                self.state_text.set_color(st_color)

                # Redraw
                self.canvas.draw()

            except Exception as e:
                print("Error reading/parsing microcontroller data:", e)

        return self.line_collection,

    # -------------------- MULTIMETER VALIDATION UPDATE --------------------
    def update_validation(self):
        """
        Reads voltage data from the multimeter (COM12), converts it to temperature,
        and compares to the microcontroller temperature. Logs results to console
        and saves them for export.
        """
        if self.ser_multimeter:
            try:
                # Try to read one line
                strin_bytes = self.ser_multimeter.readline()
                strin = strin_bytes.decode(errors='ignore')
                # Discard next line if needed
                self.ser_multimeter.readline()

                # Issue measurement command again
                self.ser_multimeter.write(b"MEAS1?\r\n")

                if len(strin) > 1 and strin[1] == '>':
                    strin_bytes = self.ser_multimeter.readline()
                    strin = strin_bytes.decode(errors='ignore')

                strin_clean = strin.replace("VDC", "").strip()
                if strin_clean:
                    self.dmm_out_var.set(strin.strip())

                    try:
                        # Convert volts → mV
                        val_mV = float(strin_clean) * 1000.0
                        valid_val = True
                    except:
                        valid_val = False

                    try:
                        cj = float(self.cj_temp_var.get())
                    except:
                        cj = 0.0

                    if valid_val:
                        # Convert mV to temperature
                        ktemp = round(kconvert.mV_to_C(val_mV, cj), 1)
                        if ktemp < -200:
                            self.temp_var.set("UNDER")
                        elif ktemp > 1372:
                            self.temp_var.set("OVER")
                        else:
                            self.temp_var.set(str(ktemp))

                        # Compare with microcontroller reading
                        if len(self.temp_data) > 0:
                            micro_temp = self.temp_data[-1]
                            # The last state is in state_data[-1] if available
                            state_val = self.state_data[-1] if len(self.state_data)>0 else 0
                            error_val = round(abs(ktemp - micro_temp), 2)

                            # For console table
                            if not self.printed_header:
                                print(f"{'Micro Temp':<20} {'State':<10} "
                                      f"{'Multimeter Temp':<20} {'Error':<20}")
                                self.printed_header = True

                            micro_f = f"{micro_temp:09.4f}"
                            multi_f = f"{ktemp:09.4f}"
                            err_f   = f"{error_val:09.4f}"
                            print(f"{micro_f:<20} {state_val:<10} {multi_f:<20} {err_f:<20}")

                            self.data_rows.append((micro_f, str(state_val), multi_f, err_f))
                    else:
                        self.temp_var.set("----")
                else:
                    self.temp_var.set("----")

            except Exception as e:
                print("Error updating validation:", e)

        # Schedule the next read
        self.after(500, self.update_validation)

    # -------------------- EXPORT DATA TO EXCEL --------------------
    def export_data_to_excel(self, filename="temperature_data.xlsx"):
        try:
            wb = Workbook()
            ws = wb.active
            ws.title = "Temperature Data"
            ws.append(["Micro Temp", "State", "Multimeter Temp", "Error"])
            for row in self.data_rows:
                ws.append(list(row))
            wb.save(filename)
            print(f"Data exported successfully to {filename}")
            return filename
        except Exception as e:
            print("Error exporting data to Excel:", e)
            return None

    # -------------------- SEND EMAIL WITH ATTACHMENT --------------------
    def send_email(self, attachment_path):
        recipient = self.recipient_email_var.get().strip()
        if not recipient:
            print("No recipient email provided; skipping email send.")
            return

        try:
            msg = EmailMessage()
            msg["Subject"] = EMAIL_SUBJECT
            msg["From"]    = FROM_EMAIL
            msg["To"]      = recipient
            msg.set_content(EMAIL_BODY)

            with open(attachment_path, "rb") as f:
                file_data = f.read()
                file_name = f.name

            msg.add_attachment(
                file_data,
                maintype="application",
                subtype="vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                filename=file_name
            )

            with smtplib.SMTP_SSL(SMTP_SERVER, SMTP_PORT) as server:
                server.login(SMTP_USERNAME, SMTP_PASSWORD)
                server.send_message(msg)

            print("Email sent successfully.")
        except Exception as e:
            print("Error sending email:", e)

    # -------------------- HANDLE APPLICATION CLOSE --------------------
    def on_close(self):
        """
        Stop all loops, close serial ports, export data to Excel, email the file,
        stop music if playing, and destroy the GUI window.
        """
        if self.ser_micro:
            self.ser_micro.close()
        if self.ser_multimeter:
            self.ser_multimeter.close()

        pygame.mixer.music.stop()
        excel_file = self.export_data_to_excel()
        if excel_file:
            self.send_email(excel_file)

        self.destroy()

# -------------- MAIN ENTRY POINT --------------
if __name__ == '__main__':
    app = CombinedApp()
    app.mainloop()
