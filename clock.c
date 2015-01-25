
int bissextile(int year) {
	if (year % 4) {
		return 0;
	}
	if (year % 100 == 0 && year % 400)
		return 0;
	return 1;
}

int main(void) {
	int sec = 0, min = 0, hour = 0;
	int day = 0, month = 0, year = 0;
	while (1) {
		sec++;
		if (sec%60 == 0)
			min++;
		if (min == 60) {
			min = 0;
			hour++;
		}
		if (hour == 24) {
			hour = 0;
			day++;
		}
		if((month == 0  && day == 32)
		|| (month == 1  && day == 29 + bissextile(year))
	   	|| (month == 2  && day == 32)
		|| (month == 3  && day == 31)
		|| (month == 4  && day == 32)
		|| (month == 5  && day == 31)
		|| (month == 6  && day == 32)
		|| (month == 7  && day == 32)
		|| (month == 8  && day == 31)
		|| (month == 9  && day == 32)
		|| (month == 10 && day == 31)
		|| (month == 11 && day == 32)) {
			day = 0;
			month++;
		}
		if (month == 12) {
			month = 0;
			year++;
		}
	}
	return 0;
}
