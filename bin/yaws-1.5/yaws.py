#/!usr/bin/python3
# -*- coding: utf-8 -*-
#================================================================================#
#  yaws.py                                                                       #
#  Yet Another Weather Script                                                    #
#  !!! WORKS ONLY OUTSIDE NORWAY !!!                                             #  # TODO: FIX THIS!!!
#  Script for weather forecasts using the REST style API from The Norwegian      #
#  Meteorological institute, outputs to command line.                            #
#--------------------------------------------------------------------------------#
# This program is free software; you can redistribute it and/or                  #
# modify it under the terms of the GNU General Public License                    #
# as published by the Free Software Foundation; either version 3                 #
# of the License, or (at your option) any later version.                         #
#                                                                                #
# This program is distributed in the hope that it will be useful,                #
# but WITHOUT ANY WARRANTY; without even the implied warranty of                 #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                   #
# GNU General Public License for more details.                                   #
#                                                                                #
# You should have received a copy of the GNU General Public License              #
# along with this program; if not, write to the Free Software                    #
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. #
#================================================================================#

__appname__ = 'YAWS'
__author__ = 'Boisei0'
__version__ = '1.5.1'

import argparse
import datetime
import gettext
import locale
import operator
import os
import requests
import sys
import xml.etree.ElementTree

reload(sys)
sys.setdefaultencoding('utf-8')

# default to standard locale translation
domain = __file__.replace(os.path.dirname(__file__) + "/", "").replace(".py", "")
locale_directory = os.path.dirname(os.path.abspath(__file__)) + "/locale"
gettext.bindtextdomain(domain, locale_directory)
gettext.textdomain(domain)
gettext.install(domain)


class CommandlineParser:
    def __init__(self):
        self.parser = argparse.ArgumentParser(description=u'YAWS: Yet Another Weather Script')
        self.parser.add_argument('lat', type=float, help=u'latitude of the location')
        self.parser.add_argument('lon', type=float, help=u'longitude of the location')
        self.parser.add_argument('--msl', type=int, help=u'meters above sea level of the location')
        self.parser.add_argument("-t", "--template", dest="template", metavar="FILE", help=u"define a template file to generate output in one call. A displayable item in the file is in the form [--datatype=HT --startday=1]. The following are possible options within each item: --datatype, --startday, --endday, --imperial, --beaufort, --metrespersecond, --shortweekday, --hideunits, --hidedegreesymbol, --night. Note that the short forms of the options are not supported! If any of these options is set from the commandline, it sets the default value of the option for all template items.")
        self.parser.add_argument("-L", "--locale", dest="locale", help=u"override the system locale for language output (en=english, nl=dutch, more to come)")
        self.parser.add_argument('-d', '--datatype', dest='datatype', default='HT', help=u"[default: HT] The data type options are: DW (Day of Week), WI (Weather Icon Path), LT (Low Temp), HT (High Temp), CC (Current Conditions), CT (Conditions Text), HM (Humidity), WD (Wind Direction), WA (Wind Angle - in degrees), WS (Wind Speed), CN (City Name), CO (Country), SR (SunRise), SS (SunSet), DL (DayLight), MP (Moon Phase),  BR (Barometer Reading), BD (Barometer Description), DP (Dew Point), WM (weather map fetch and image path returned),  LU (Last Update), LF (Last Fetch). Not applicable at command line when using templates.")
        self.parser.add_argument('-s', "--startday", dest="startday", type=int, metavar="NUMBER", help=u"define the starting day number, if omitted current conditions are output. Not applicable at command line when using templates.")
        self.parser.add_argument("-e", "--endday", dest='endday', type=int, metavar='NUMBER', help=u"define the ending day number, if omitted only starting day data is output. Not applicable at command line when using templates.")
        self.parser.add_argument('-i', '--imperial', dest='imperial', default=False, action='store_true', help=u'request imperial units, if omitted output is in metric.')
        self.parser.add_argument('-b', '--beaufort', dest='beaufort', default=False, action='store_true', help=u'request beaufort scale for wind speeds, if omitted output is either metric/imperial.')
        self.parser.add_argument("-M", "--metrespersecond", dest="metrespersecond", default=False, action="store_true", help=u"request metres per second for wind speeds, if omitted output is either metric/imperial.")
        self.parser.add_argument("-n", "--night", dest="night", default=False, action="store_true", help=u"switch output to night data, if omitted day output will be output.")
        self.parser.add_argument("-x", "--hidedegreesymbol", dest="hidedegreesymbol", default=False, action="store_true", help=u"Hide the degree symbol used with temperature output")
        self.parser.add_argument("-u", "--hideunits", dest="hideunits", default=False, action="store_true", help=u"Hide units such as mph or C, degree symbols (°) are still shown.")
        self.parser.add_argument("-w", "--shortweekday", dest="shortweekday", default=False, action="store_true", help=u"Shorten the day of week data type to 3 characters.")
        self.parser.add_argument('--version', action='version', version='{} v{} by {}'.format(__appname__, __version__,
                                                                                              __author__))

    def parse_args(self):
        return self.parser.parse_args()


class Converter:
    def __init__(self):
        pass

    @staticmethod
    def celcius_to_fahrenheit(celcius, dp=0):
        celcius = float(celcius)
        return round(((celcius * 9.0) / 5.0) + 32, dp)

    @staticmethod
    def mps_to_kph(mps, dp=0):
        mps = float(mps)
        return round(mps * 3.6, dp)

    @staticmethod
    def mps_to_mph(mps, dp=0):
        mps = float(mps)
        return round(mps * 2.237, dp)


class Sunrise:
    """Module for sun- and moonrise using the REST style API from The Norwegian Meteorological institute"""
    def __init__(self, lat, lon, date=None, date_from=None, date_to=None):
        if date is not None:
            resp = requests.get('http://api.met.no/weatherapi/sunrise/1.0/?lat={};lon={};date={}'.format(lat, lon, date))
        elif date_from is not None and date_to is not None:
            resp = requests.get('http://api.met.no/weatherapi/sunrise/1.0/?lat={};lon={};from={};to={}'.format(lat, lon, date_from, date_to))
        else:
            today = datetime.datetime.today()
            date = today.strftime('%Y-%m-%d')
            resp = requests.get('http://api.met.no/weatherapi/sunrise/1.0/?lat={};lon={};date={}'.format(lat, lon, date))

        if resp.status_code == 200:
            self.root = xml.etree.ElementTree.fromstring(resp.text)
        else:
            raise Exception

    def list_dates(self):
        dates = []
        for child in self.root:
            if child.tag != 'meta':
                dates.append(child.attrib['date'])
        return dates

    def get_index_by_date(self, date):
        if date in self.list_dates():
            index = 0
            for child in self.root:
                if child.tag == 'meta':
                    index += 1
                elif child.attrib['date'] == date:
                    return index
                else:
                    index += 1

    def fill_model_by_date(self, date):
        if date in self.list_dates():
            index = self.get_index_by_date(date)

            sun_rise = self.root[index][0][0].attrib['rise']
            sun_set = self.root[index][0][0].attrib['set']
            moon_rise = self.root[index][0][1].attrib['rise']
            moon_set = self.root[index][0][1].attrib['set']
            moon_phase = self.root[index][0][1].attrib['phase']

            model = SunriseModel(date, sun_rise, sun_set, moon_rise, moon_set, moon_phase)

            return model
        else:
            raise Exception

    def parse_data(self, args, model):
        # elif args.datatype in ['DL', 'MP', 'MR', 'MS', 'SR', 'SS']:
        if args.datatype == 'DL':
            return self.get_daylight(model.sun_rise, model.sun_set)
        elif args.datatype == 'MP':
            return model.moon_phase
        elif args.datatype == 'MR':
            return datetime.datetime.strptime(model.moon_rise, '%Y-%m-%dT%H:%M:%SZ').strftime('%H:%M')
        elif args.datatype == 'MS':
            return datetime.datetime.strptime(model.moon_set, '%Y-%m-%dT%H:%M:%SZ').strftime('%H:%M')
        elif args.datatype == 'SR':
            return datetime.datetime.strptime(model.sun_rise, '%Y-%m-%dT%H:%M:%SZ').strftime('%H:%M')
        elif args.datatype == 'SS':
            return datetime.datetime.strptime(model.sun_set, '%Y-%m-%dT%H:%M:%SZ').strftime('%H:%M')
        else:
            raise Exception

    @staticmethod
    def get_daylight(sun_rise, sun_set):
        sun_set = datetime.datetime.strptime(sun_set, '%Y-%m-%dT%H:%M:%SZ')
        sun_rise = datetime.datetime.strptime(sun_rise, '%Y-%m-%dT%H:%M:%SZ')

        return timedelta_dl_to_time((sun_set - sun_rise))


class SunriseModel:
    def __init__(self, date, sun_rise, sun_set, moon_rise, moon_set, moon_phase):
        self.date = date
        self.sun_rise = sun_rise
        self.sun_set = sun_set
        self.moon_rise = moon_rise
        self.moon_set = moon_set
        self.moon_phase = moon_phase


class Template:
    def __init__(self, lat, lon, msl, forecast, template):
        self.lat = lat
        self.lon = lon
        self.msl = msl
        self.forecast = forecast
        self.template = template

    def parse(self):
        for command in self.get_all_commands():
            output = self.parse_command(command)
            try:
                self.template = self.template.replace(command, output)
            except TypeError:
                pass
        return self.template

    def parse_command(self, full_command):
        full_command = full_command[1:-1]
        full_args = full_command.split(' ')

        datatype = None
        startday = None
        endday = None
        imperial = None
        beaufort = None
        metrespersecond = None
        shortweekday = None
        hideunits = None
        hidedegreesymbol = None
        night = None

        for argument in full_args:
            if argument[:-2] == '--datatype=':
                datatype = argument[-2:]
            elif argument[:-1] == '--startday=':
                startday = int(argument[-1:])
            elif argument[:-1] == '--endday=':
                endday = int(argument[-1:])
            elif argument == '--imperial':
                imperial = True
            elif argument == '--beaufort':
                beaufort = True
            elif argument == '--metrespersecond':
                metrespersecond = True
            elif argument == '--shortweekday':
                shortweekday = True
            elif argument == '--hideunits':
                hideunits = True
            elif argument == '--hidedegreesymbol':
                hidedegreesymbol = True
            elif argument == '--night':
                night = True
            else:
                raise Exception

        if datatype is None:
            datatype = 'HT'
        if startday is None:
            startday = 0
        if endday is None:
            endday = startday
        if imperial is None:
            imperial = False
        if beaufort is None:
            beaufort = False
        if metrespersecond is None:
            metrespersecond = False
        if shortweekday is None:
            shortweekday = False
        if hideunits is None:
            hideunits = False
        if hidedegreesymbol is None:
            hidedegreesymbol = False
        if night is None:
            night = False

        args = TemplateArgs(self.lat, self.lon, self.msl, datatype, startday, endday, imperial, beaufort,
                            metrespersecond, shortweekday, hideunits, hidedegreesymbol, night)

        if range(startday, endday) == list():
            output = parse_forecast_arguments(args, self.forecast, days=startday)
            return output
        else:
            output = []
            for day in range(startday, endday):
                output.append(parse_forecast_arguments(args, self.forecast, days=day))
            return output

    def get_all_commands(self):
        commands = []
        current_pos = 0
        while self.template.find('[', current_pos) != -1:  # commands left in file
            pos_command_start = self.template.find('[', current_pos)
            pos_command_end = self.template.find(']', current_pos)

            if pos_command_end == -1:  # malformed command
                raise Exception

            command = self.template[pos_command_start:pos_command_end + 1]
            commands.append(command)
            current_pos = pos_command_end + 1
        return commands


class TemplateArgs:
    def __init__(self, lat, lon, msl, datatype, startday, endday, imperial, beaufort, metrespersecond, shortweekday,
                 hideunits, hidedegreesymbol, night):
        self.lat = lat
        self.lon = lon
        self.msl = msl
        self.datatype = datatype
        self.startday = startday
        self.endday = endday
        self.imperial = imperial
        self.beaufort = beaufort
        self.metrespersecond = metrespersecond
        self.shortweekday = shortweekday
        self.hideunits = hideunits
        self.hidedegreesymbol = hidedegreesymbol
        self.night = night


# start ignoring translations required at runtime
def _(text):
    return text


class TextData:
    def __init__(self):
        pass

    barometric_description = {
        0: _('Stormy'),
        1: _('Rain'),
        2: _('Change'),
        3: _('Fair'),
        4: _('Very dry')
    }

    weekdays = {
        0: _('Monday'),
        1: _('Tuesday'),
        2: _('Wednesday'),
        3: _('Thursday'),
        4: _('Friday'),
        5: _('Saturday'),
        6: _('Sunday')
    }

    weekdays_short = {
        0: _('Mon'),
        1: _('Tue'),
        2: _('Wed'),
        3: _('Thu'),
        4: _('Fri'),
        5: _('Sat'),
        6: _('Sun')
    }

    weather_conditions = {
        1: _('Sun'),
        2: _('Light cloudy'),
        3: _('Partly cloudy'),
        4: _('Cloudy'),
        5: _('Light rain and sun'),
        6: _('Light rain with thunder and sun'),
        7: _('Sleet and sun'),
        8: _('Snow and sun'),
        9: _('Light rain'),
        10: _('Rain'),
        11: _('Rain and thunder'),
        12: _('Sleet'),
        13: _('Snow'),
        14: _('Snow and thunder'),
        15: _('Fog'),
        16: _('Sun'),
        17: _('Light cloudy'),
        18: _('Light rain and sun'),
        19: _('Snow and sun'),
        20: _('Sleet with sun and thunder'),
        21: _('Snow with sun and thunder'),
        22: _('Light rain and thunder'),
        23: _('Sleet and thunder')
    }

# end ignoring translations
del _


class WeatherForecast:
    """Module for weather forecasts using the REST style API from The Norwegian Meteorological institute"""
    def __init__(self, lat, lon, msl=None):
        if not msl:
            resp = requests.get('http://api.met.no/weatherapi/locationforecast/1.9/?lat={};lon={}'.format(lat, lon))
        else:
            resp = requests.get('http://api.met.no/weatherapi/locationforecast/1.9/?lat={};lon={};msl={}'.format(lat, lon, msl))

        if resp.status_code == 200:
            self.root = xml.etree.ElementTree.fromstring(resp.text)
        else:
            raise Exception

    def list_precipitation_times(self):
        times = []
        for child in self.root[1]:
            if child.attrib['from'] != child.attrib['to']:  # only precipitation forecasts
                times.append(child.attrib['from'])
        return times

    def list_all_times(self):
        times = []
        for child in self.root[1]:
            times.append(child.attrib['from'])
        return times

    def list_times(self):
        times = []
        for child in self.root[1]:
            if child.attrib['from'] == child.attrib['to']:  # full forecast
                times.append(child.attrib['from'])
        return times

    @staticmethod
    def get_barometric_description(pressure):
        pressure = float(pressure)
        if pressure < 965:
            return _(TextData.barometric_description[0])
        elif pressure < 980:
            return _(TextData.barometric_description[1])
        elif pressure < 1015:
            return _(TextData.barometric_description[2])
        elif pressure < 1030:
            return _(TextData.barometric_description[3])
        else:
            return _(TextData.barometric_description[4])

    def get_forecast_index_t12_by_date(self, date):
        timestamp = '{}T12:00:00Z'.format(date)
        return self.get_index_by_timestamp(timestamp)

    def get_indices_by_date(self, date):
        indices = []
        index = 0
        for child in self.root[1]:
            full_date = datetime.datetime.strptime(child.attrib['from'], '%Y-%m-%dT%H:%M:%SZ')
            if full_date.strftime('%Y-%m-%d') == date and child.attrib['from'] != child.attrib['to']:
                indices.append(index)
            index += 1
        return indices

    def get_forecast_indices_by_date(self, date):
        indices = []
        index = 0
        for child in self.root[1]:
            full_date = datetime.datetime.strptime(child.attrib['from'], '%Y-%m-%dT%H:%M:%SZ')
            if full_date.strftime('%Y-%m-%d') == date and child.attrib['from'] == child.attrib['to']:
                indices.append(index)
            index += 1
        return indices

    def get_index_by_timestamp(self, timestamp):
        if timestamp in self.list_times():
            index = 0
            for child in self.root[1]:
                if child.attrib['from'] == timestamp:
                    return index
                else:
                    index += 1

    def get_low_temperature_by_date(self, date):
        indices = self.get_forecast_indices_by_date(date)
        temperature_list = []
        for index in indices:
            for i in range(0, 11):
                try:
                    if self.root[1][index][0][i].tag == 'temperature':
                        temperature_list.append(self.root[1][index][0][i].attrib['value'])
                except IndexError:
                    pass
        return min(temperature_list)

    def get_mean_weather_symbol_by_date(self, date):
        indices = self.get_indices_by_date(date)
        symbol_list = []
        for index in indices:
            try:
                symbol_list.append(self.root[1][index][0][1].attrib['number'])
            except IndexError:
                pass
        return self.get_mean_weather_symbol(symbol_list)

    @staticmethod
    def get_mean_weather_symbol(symbol_list):
        symbol_count_dict = {
            0: 0,
            1: 0,
            2: 0,
            3: 0,
            4: 0,
            5: 0,
            6: 0,
            7: 0,
            8: 0,
            9: 0,
            10: 0,
            11: 0,
            12: 0,
            13: 0,
            14: 0,
            15: 0,
            16: 0,
            17: 0,
            18: 0,
            19: 0,
            20: 0,
            21: 0,
            22: 0,
            23: 0
        }

        for item in symbol_list:
            symbol_count_dict[int(item)] += 1

        return max(symbol_count_dict.iteritems(), key=operator.itemgetter(1))[0]

    def get_last_update(self):
        return self.root[0][0].attrib['runended']

    def fill_model_by_timestamp(self, timestamp):
        if timestamp in self.list_times():
            index = self.get_index_by_timestamp(timestamp)

            temperature = 0
            wind_angle = 0
            wind_direction = 0
            wind_speed_mps = 0
            wind_speed_bft = 0
            humidity = 0
            pressure = 0
            cloudiness = 0
            fog = 0
            low_clouds = 0
            medium_clouds = 0
            high_clouds = 0
            dewpoint = 0
            precipitation = 0

            for i in range(0, 11):
                try:
                    item = self.root[1][index][0][i]
                    tag = item.tag
                    if tag == 'temperature':
                        temperature = item.attrib['value']
                    elif tag == 'windDirection':
                        wind_angle = item.attrib['deg']
                        wind_direction = item.attrib['name']
                    elif tag == 'windSpeed':
                        wind_speed_mps = item.attrib['mps']
                        wind_speed_bft = item.attrib['beaufort']
                    elif tag == 'humidity':
                        humidity = item.attrib['value']
                    elif tag == 'pressure':
                        pressure = item.attrib['value']
                    elif tag == 'cloudiness':
                        cloudiness = item.attrib['percent']
                    elif tag == 'fog':
                        fog = item.attrib['percent']
                    elif tag == 'lowClouds':
                        low_clouds = item.attrib['percent']
                    elif tag == 'mediumClouds':
                        medium_clouds = item.attrib['percent']
                    elif tag == 'highClouds':
                        high_clouds = item.attrib['percent']
                    elif tag == 'dewpointTemperature':
                        dewpoint = item.attrib['value']
                    elif tag == 'precipitation':
                        precipitation = item.attrib['value']
                    else:
                        # do not raise exception; add Norway only items later
                        pass
                except IndexError:
                    pass

            date = datetime.datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%SZ').strftime('%Y-%m-%d')
            low_temperature = self.get_low_temperature_by_date(date)

            model = ForecastModel(timestamp, temperature, wind_angle, wind_direction, wind_speed_mps, wind_speed_bft,
                                  humidity, pressure, cloudiness, fog, low_clouds, medium_clouds, high_clouds,
                                  dewpoint, low_temperature, precipitation)
            return model
        else:
            raise Exception

    def parse_forecast(self, args, model, date):
        # if args.datatype in ['BR', 'BD', 'CC', 'CT', 'DP', 'HM', 'HT', 'LU', 'LT', 'PC', 'WA', 'WD', 'WI', 'WS']:
        if args.datatype == 'BR':
            if args.hideunits:
                return '{}'.format(model.pressure)
            else:
                return '{} hPa'.format(model.pressure)
        elif args.datatype == 'BD':
            return self.get_barometric_description(model.pressure)
        elif args.datatype in ['CC', 'CT']:
            return _(TextData.weather_conditions[self.get_mean_weather_symbol_by_date(date)])
        elif args.datatype == 'DP':
            if args.imperial:
                if args.hidedegreesymbol:
                    if args.hideunits:
                        return u'{}'.format(Converter.celcius_to_fahrenheit(model.dewpoint, 1))
                    else:
                        return u'{}F'.format(Converter.celcius_to_fahrenheit(model.dewpoint, 1))
                else:
                    if args.hideunits:
                        return u'{}°'.format(Converter.celcius_to_fahrenheit(model.dewpoint, 1))
                    else:
                        return u'{}°F'.format(Converter.celcius_to_fahrenheit(model.dewpoint, 1))
            else:
                if args.hidedegreesymbol:
                    if args.hideunits:
                        return u'{}'.format(model.dewpoint)
                    else:
                        return u'{}C'.format(model.dewpoint)
                else:
                    if args.hideunits:
                        return u'{}°'.format(model.dewpoint)
                    else:
                        return u'{}°C'.format(model.dewpoint)
        elif args.datatype == 'HM':
            if args.hideunits:
                return '{}'.format(model.humidity)
            else:
                return '{}%'.format(model.humidity)
        elif args.datatype == 'HT':
            if args.imperial:
                if args.hidedegreesymbol:
                    if args.hideunits:
                        return u'{}'.format(Converter.celcius_to_fahrenheit(model.temperature, 1))
                    else:
                        return u'{}F'.format(Converter.celcius_to_fahrenheit(model.temperature, 1))
                else:
                    if args.hideunits:
                        return u'{}°'.format(Converter.celcius_to_fahrenheit(model.temperature, 1))
                    else:
                        return u'{}°F'.format(Converter.celcius_to_fahrenheit(model.temperature, 1))
            else:
                if args.hidedegreesymbol:
                    if args.hideunits:
                        return u'{}'.format(model.temperature)
                    else:
                        return u'{}C'.format(model.temperature)
                else:
                    if args.hideunits:
                        return u'{}°'.format(model.temperature)
                    else:
                        return u'{}°C'.format(model.temperature)
        elif args.datatype == 'LU':
            return self.get_last_update()
        elif args.datatype == 'LT':
            if args.imperial:
                if args.hidedegreesymbol:
                    if args.hideunits:
                        return u'{}'.format(Converter.celcius_to_fahrenheit(model.low_temperature, 1))
                    else:
                        return u'{}F'.format(Converter.celcius_to_fahrenheit(model.low_temperature, 1))
                else:
                    if args.hideunits:
                        return u'{}°'.format(Converter.celcius_to_fahrenheit(model.low_temperature, 1))
                    else:
                        return u'{}°F'.format(Converter.celcius_to_fahrenheit(model.low_temperature, 1))
            else:
                if args.hidedegreesymbol:
                    if args.hideunits:
                        return u'{}'.format(model.low_temperature)
                    else:
                        return u'{}C'.format(model.low_temperature)
                else:
                    if args.hideunits:
                        return u'{}°'.format(model.low_temperature)
                    else:
                        return u'{}°C'.format(model.low_temperature)
        elif args.datatype == 'PC':
            pass
        elif args.datatype == 'PN':
            return u'{} mm'.format(model.precipitation)
        elif args.datatype == 'WA':
            if args.hidedegreesymbol:
                return u'{}'.format(model.wind_angle)
            else:
                return u'{}°'.format(model.wind_angle)
        elif args.datatype == 'WD':
            return model.wind_direction
        elif args.datatype == 'WI':
            if args.night:
                return '{}/night-{}.png'.format(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'images'),
                                         self.get_mean_weather_symbol_by_date(date))
            else:
                return '{}/day-{}.png'.format(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'images'),
                                         self.get_mean_weather_symbol_by_date(date))
        elif args.datatype == 'WS':
            if args.beaufort:
                if args.hideunits:
                    return '{}'.format(model.wind_speed_bft)
                else:
                    return '{} Bft'.format(model.wind_speed_bft)
            elif args.imperial:
                if args.hideunits:
                    return '{}'.format(Converter.mps_to_mph(model.wind_speed_mps))
                else:
                    return '{} mph'.format(Converter.mps_to_mph(model.wind_speed_mps))
            else:
                if args.hideunits:
                    return '{}'.format(Converter.mps_to_kph(model.wind_speed_mps))
                else:
                    return '{} km/h'.format(Converter.mps_to_kph(model.wind_speed_mps))
        else:
            raise Exception


class ForecastModel:
    def __init__(self, timestamp, temperature, wind_angle, wind_direction, wind_speed_mps, wind_speed_bft, humidity, pressure,
                 cloudiness, fog, low_clouds, medium_clouds, high_clouds, dewpoint, low_temperature, precipitation):
        self.timestamp = timestamp
        self.temperature = temperature
        self.wind_angle = wind_angle
        self.wind_direction = wind_direction
        self.wind_speed_mps = wind_speed_mps
        self.wind_speed_bft = wind_speed_bft
        self.humidity = humidity
        self.pressure = pressure
        self.cloudiness = cloudiness
        self.fog = fog
        self.low_clouds = low_clouds
        self.medium_clouds = medium_clouds
        self.high_clouds = high_clouds
        self.dewpoint = dewpoint
        self.low_temperature = low_temperature
        self.precipitation = precipitation


def isNumeric(string):
    try:
        dummy = float(string)
        return True
    except:
        return False


def parse_forecast_arguments(args, forecast, days=0):
    date_delta = datetime.datetime.utcnow() + datetime.timedelta(days=days)
    date = timedelta_to_date(date_delta)

    if args.datatype in ['BR', 'BD', 'CC', 'CT', 'DP', 'HM', 'HT', 'LU', 'LT', 'PC', 'PN', 'WA', 'WD', 'WI', 'WS']:
        index = forecast.get_forecast_index_t12_by_date(date)
        if index is None:
            index = 0
        model = forecast.fill_model_by_timestamp(forecast.list_all_times()[index])

        return forecast.parse_forecast(args, model, date)

    elif args.datatype in ['DL', 'MP', 'MR', 'MS', 'SR', 'SS']:
        sunrise = Sunrise(args.lat, args.lon, date)
        model = sunrise.fill_model_by_date(date)

        return sunrise.parse_data(args, model)

    elif args.datatype == 'DW':
        date_dt = datetime.datetime.strptime(date, '%Y-%m-%d')
        if args.shortweekday:
            return _(TextData.weekdays_short[date_dt.weekday()])
        else:
            return _(TextData.weekdays[date_dt.weekday()])

    elif args.datatype == 'LF':
        return datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    elif args.datatype in ['CN', 'CO']:
        city, country = reverse_geocode(args.lat, args.lon)
        if args.datatype == 'CN':
            return city
        elif args.datatype == 'CO':
            return country
        else:
            raise Exception

    elif args.datatype == 'WM':
        return get_weather_map(800, 400)


def reverse_geocode(lat, lon):
    url = 'http://maps.googleapis.com/maps/api/geocode/json?latlng={},{}&sensor=false'.format(lat, lon)
    resp = requests.get(url)
    if resp.status_code != 200:
        raise Exception
    data = resp.json()
    city = data['results'][0]['address_components'][2]['long_name']  # TODO: TEST
    country = data['results'][0]['address_components'][5]['long_name']  # TODO: TEST
    return city, country


def get_weather_map(width, height):
    url = 'http://api.met.no/weatherapi/geosatellite/1.3/?area=global;width={};height={}'.format(width, height)
    resp = requests.get(url, stream=True)
    if resp.status_code != 200:
        raise Exception

    image_path = '/tmp/weathermap.jpg'
    with open(image_path, 'wb') as image_file:
        image_file.write(resp.content)
    return image_path


def timedelta_to_date(delta):
    full_date = datetime.datetime.strptime(str(delta), '%Y-%m-%d %H:%M:%S.%f')
    return full_date.strftime('%Y-%m-%d')


def timedelta_dl_to_time(delta, hide_seconds=True):
    full_date = datetime.datetime.strptime(str(delta), '%H:%M:%S')
    if hide_seconds:
        return full_date.strftime('%H:%M')
    else:
        return str(delta)


def main():
    parser = CommandlineParser()
    args = parser.parse_args()

    try:
        # set the locale
        if not args.locale:
            language = locale.getdefaultlocale()[0][0:2]
        else:
            language = args.locale

        if language != 'en':
            if gettext.find(domain, locale_directory, languages=[language]) is not None:
                try:
                    trans = gettext.translation(domain, locale_directory, languages=[language])
                    trans.install(unicode=True)
                except Exception:
                    pass
    except Exception:
        pass

    if args.startday is None:
        startday = 0
    else:
        startday = int(args.startday)

    if args.endday is None:
        endday = startday
    else:
        endday = int(args.endday)

    forecast = WeatherForecast(args.lat, args.lon, args.msl)

    if args.template is not None:
        with open(args.template, 'r') as f:
            template_text = f.read()

        template = Template(args.lat, args.lon, args.msl, forecast, template_text)
        print(template.parse())
    else:
        if range(startday, endday) == list():
            print(parse_forecast_arguments(args, forecast, days=startday))
        else:
            for day in range(startday, endday):
                print(parse_forecast_arguments(args, forecast, days=day))

if __name__ == '__main__':
    main()
    sys.exit(0)
