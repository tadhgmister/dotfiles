import time
import subprocess

def low_power_alert(time_remaining):
    """somehow informs the user that the battery is running low
    then returns the time to sleep (in minutes) to wait before checking again
    note that a time less than MIN_SLEEP_TIME will still wait the min sleep time.
    """
    p = subprocess.Popen(["festival", "--tts"], stdin=subprocess.PIPE)
    p.stdin.write(b"your battery will last %d minutes before fainting\n"%int(time_remaining))
    p.stdin.close()
    return 0 # just use min sleep time


def read_battery_signal(name, BATTERY_FILEPATH = "/sys/class/power_supply/BAT1/{}"):
    "reads a file in the /sys folder for the battery, with the content stripped"
    with open(BATTERY_FILEPATH.format(name), 'r') as f:
        return f.read().strip()

def is_discharging():
    "return true if the battery is currently discharging"
    return "Discharging" == read_battery_signal("status")

full_charge = int(read_battery_signal("charge_full"))

def time_remaining():
    "returns the number of minutes expected at the present discharge rate"
    # according to https://www.kernel.org/doc/html/latest/power/power_supply_class.html
    # charge_now is in microAh and current_now is in microA
    # so dividing we get number of hours, we want minutes
    return int(read_battery_signal("charge_now")) / int(read_battery_signal("current_now")) * 60

RATIO_TO_CHECK = 0.6 # check again when time remaining will be this percentage of what is measured, must be between 0 and 1 where higher means it will check more often, lower means it will be more efficient but if power draw changes drastically it might miss the oppertunity
# all times are in minutes
TIME_TO_SLEEP_WHEN_CHARGING = 15
ALERT_TIME_THRESHOLD = 30

MIN_SLEEP_TIME = 2 # when ratio is small enough don't check mre often that this frequency
MAX_SLEEP_TIME = 30 # if a check happens to be when there is very low usage then the estimate is very high, limit the rate of checking to some max as well
time_to_sleep = 0 # doesn't actually matter, will still wait min_sleep_time at first activation
while True:
    print(f"sleeping for {time_to_sleep} mins")
    time.sleep(min(MAX_SLEEP_TIME,max(MIN_SLEEP_TIME, time_to_sleep)) * 60)
    if not is_discharging():
        time_to_sleep = TIME_TO_SLEEP_WHEN_CHARGING
        continue
    t = time_remaining()
    if t < ALERT_TIME_THRESHOLD:
        time_to_sleep = low_power_alert(t)
        continue
    # otherwise use ratio to exponentially check more frequently when time is running low
    time_to_sleep = (t-ALERT_TIME_THRESHOLD) * RATIO_TO_CHECK
    
