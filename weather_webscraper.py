import pandas as pd 
import requests 
from bs4 import BeautifulSoup
import datetime
from datetime import date, timedelta
import sys
out_dir = './weather_data/'

def daterange(date1, date2):
    for n in range(int ((date2 - date1).days)+1):
        yield date1 + timedelta(n)


url_base = 'https://www.weatherforyou.com/reports/index.php?config=&forecast=pass&pass=archive&pands=&zipcode=&place=manhattan&state=ny&country=us&county=36061&icao=KNYC&hwvMetarIcao=KNYC&zone=NYZ072&cwa=OKX&month=%s&day=%s&year=%s&dosubmit=Go'
with open(out_dir + 'log_year.txt', 'w+') as f:
	f.write('LOGFILE: Scraping data for year: \n')
	f.write(url_base)

# dates generation
start_dt = date(2014, 1, 1)
end_dt = date(2018,12,31)
dates = [dt.strftime('%Y-%m-%d') for dt in daterange(start_dt, end_dt)]
	
col_names = ['date', 'time', 'weather_description', 'temp_in_f', 'dewpt_in_f', 'humidity_in_%', 'pressure', 'precipitation_in_inches', 'wind_direction', 'wind_speed_in_mph']		
final_df = pd.DataFrame(columns = col_names)

for i, date in enumerate(dates):
	month = date.split('-')[1]
	day = date.split('-')[2]

	# open url for the corresponding date
	r = requests.get(url_base % (month, day)) # check the format 
	print(url_base % (month,day))

	with open(out_dir + 'log_year.txt', 'w') as file:
		file.write('Date: {} || Status: {}\n'.format(date, r.status_code))

	if r.status_code == 200 : # everything is ok 
		# get the data from beautiful soup 
		soup = BeautifulSoup(r.text, 'html.parser')
		table = soup.find('table', {'width':'660', 'cellspacing':'1', 'cellpadding':'2', 'border':'0'})
		table_body = table.find_all('tbody')
	
		# column names
		rows = table.find_all('tr')[1:]
		
		columns = list()
		for row in rows:
		 	cols = row.find_all('td')
		 	cols = [ele.text.strip() for ele in cols]
		 	wind_elements = cols[6].replace('\xa0', ' ').split(' ')
		 	cols.pop(6)
		 	cols = [date] + cols + wind_elements[:2]
		 	columns.append(cols)

		#close request very important
		r.close()
		df = pd.DataFrame(columns, columns = col_names)

		data_to_drop = list()
		hours = list()
		number_of_hours = range(0,23)
		
		# reformat and keep only the hours that you are interested in 		
		for index, row in df.iterrows():
			hour, rest = row[1].split(':')
			mod_hour = int(hour) % 12 
	
			if rest[-2:] == 'AM':
				if mod_hour < 10 : 
					hour = '0' + str(mod_hour) 
				else:
					hour = mod_hour
			elif rest[-2:] == 'PM':
				hour = mod_hour + 12

			hour = str(hour) + ':00:00'

			if hour not in hours: 
				hours.append(hour)
				df['time'][index] = hour
			else:
				data_to_drop.append(index)

		

		df = df.drop(index=data_to_drop)
		final_df = final_df.append(df)

		print(final_df.tail())

# write out 
final_df.to_csv (out_dir + 'weather_data.csv', index = None, header=True) 
print('SUCCESS')