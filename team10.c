/*
 * HOTEL ROOM BOOKING AND CUSTOMER MANAGEMENT SYSTEM
 * Programming for Problem Solving (PPS) project
 *
 * This is a single-user, console-based C11 application.
 * The GST and discount below are sample project settings only,
 * not official hotel tax advice.
 *
 * Versioned data files preserve older files that used other structures.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <math.h>

#define MAX_CUSTOMERS 1000
#define MAX_BOOKINGS  5000
#define MAX_ROOMS     100

#define CUSTOMER_FILE "customers_v2.dat"
#define BOOKING_FILE  "bookings_v2.dat"
#define ROOM_FILE     "rooms_v2.dat"
#define SETTINGS_FILE "hotel_settings_v2.dat"
#define BILL_FILE     "hotel_bill_history.txt"

#define MEAL_RATE       250.0
#define LAUNDRY_RATE    100.0
#define SAMPLE_GST_PERCENT      5.0
#define SAMPLE_DISCOUNT_PERCENT 0.0

#define STATUS_CHECKED_OUT 0
#define STATUS_ACTIVE      1
#define STATUS_CANCELLED   2

#define DATA_VERSION 2U

typedef struct {
    int day;
    int month;
    int year;
    long ordinal;
} Date;

typedef struct {
    int roomNumber;
    char category[20];
    double ratePerNight;
    int active;
} Room;

typedef struct {
    int customerId;
    char name[50];
    char phone[20];
} Customer;

typedef struct {
    int bookingId;
    int customerId;
    int roomNumber;
    char roomCategory[20];
    double ratePerNight;
    char checkIn[11];
    char checkOut[11];
    char actualCheckOut[11];
    int bookedNights;
    int mealDaysBooked;
    int laundryItemsBooked;
    int actualNights;
    int mealDaysUsed;
    int laundryItemsUsed;
    int status;
    double cancellationCharge;
} Booking;

typedef struct {
    char magic[8];
    uint32_t version;
    uint32_t count;
} FileHeader;

typedef struct {
    char magic[8];
    uint32_t version;
    double cancellationFeePercent;
} SettingsRecord;

static Room rooms[MAX_ROOMS] = {
    {101, "Standard", 1500.0, 1},
    {102, "Standard", 1500.0, 1},
    {201, "Deluxe",   2200.0, 1},
    {202, "Deluxe",   2200.0, 1},
    {301, "Suite",    3500.0, 1},
    {302, "Suite",    3500.0, 1}
};

static Customer customers[MAX_CUSTOMERS];
static Booking bookings[MAX_BOOKINGS];
static size_t roomCount = 6;
static size_t customerCount = 0;
static size_t bookingCount = 0;
static double cancellationFeePercent = 0.0;
static int inputEnded = 0;

static int readLine(const char *prompt, char *buffer, size_t size);
static int readInteger(const char *prompt, int minimum, int maximum);
static int readMoney(const char *prompt, double minimum, double maximum,
                     double *value);
static int readDate(const char *prompt, Date *date);
static int parseDate(const char *text, Date *date);
static long dateOrdinal(int day, int month, int year);
static int isLeapYear(int year);
static void loadData(void);
static int saveCustomers(void);
static int saveBookings(void);
static int saveRooms(void);
static int saveSettings(void);
static int findCustomer(int customerId);
static int findRoom(int roomNumber);
static int findBooking(int bookingId);
static int hasActiveBooking(int roomNumber);
static int roomHasDateConflict(int roomNumber, const Date *checkIn,
                               const Date *checkOut);
static int nextCustomerId(void);
static int nextBookingId(void);
static void displayRooms(int includeInactive);
static void registerCustomer(void);
static void createBooking(void);
static void displayBookings(void);
static void searchCustomer(void);
static void searchBooking(void);
static void manageRooms(void);
static void addRoom(void);
static void editRoom(void);
static void deactivateRoom(void);
static void changeCancellationFee(void);
static int chooseCategory(char *category, size_t size);
static void cancelBooking(void);
static void checkoutCustomer(void);
static void generateFinalBill(void);
static void writeBill(FILE *stream, const Booking *booking);
static void viewBillHistory(void);
static const char *bookingStatus(int status);
static void showMenu(void);

/* Read one bounded line. A blank line is allowed for number/date parsing,
 * while callers that need text validate it separately. */
static int readLine(const char *prompt, char *buffer, size_t size)
{
    size_t length;
    int ch;

    if (size == 0) {
        return 0;
    }

    printf("%s", prompt);
    if (fgets(buffer, (int)size, stdin) == NULL) {
        inputEnded = 1;
        buffer[0] = '\0';
        return 0;
    }

    length = strlen(buffer);
    if (length > 0 && buffer[length - 1] == '\n') {
        buffer[--length] = '\0';
        if (length > 0 && buffer[length - 1] == '\r') {
            buffer[--length] = '\0';
        }
    } else if (!feof(stdin)) {
        while ((ch = getchar()) != '\n' && ch != EOF) {
            /* Discard the remainder of an overlong line. */
        }
        printf("Input is too long. Please try again.\n");
        return -1;
    }
    return 1;
}

static int readInteger(const char *prompt, int minimum, int maximum)
{
    char line[100];
    char *start;
    char *end;
    long number;
    int result;

    while (!inputEnded) {
        result = readLine(prompt, line, sizeof(line));
        if (result == 0) {
            return -1;
        }
        if (result < 0) {
            continue;
        }

        start = line;
        while (isspace((unsigned char)*start)) {
            start++;
        }
        if (*start == '\0') {
            printf("Please enter a number.\n");
            continue;
        }

        errno = 0;
        number = strtol(start, &end, 10);
        while (isspace((unsigned char)*end)) {
            end++;
        }
        if (errno == ERANGE || start == end || *end != '\0' ||
            number < minimum || number > maximum) {
            printf("Please enter a whole number from %d to %d.\n",
                   minimum, maximum);
            continue;
        }
        return (int)number;
    }
    return -1;
}

static int readMoney(const char *prompt, double minimum, double maximum,
                     double *value)
{
    char line[100];
    char *start;
    char *end;
    double number;
    int result;

    while (!inputEnded) {
        result = readLine(prompt, line, sizeof(line));
        if (result == 0) {
            return 0;
        }
        if (result < 0) {
            continue;
        }

        start = line;
        while (isspace((unsigned char)*start)) {
            start++;
        }
        errno = 0;
        number = strtod(start, &end);
        while (isspace((unsigned char)*end)) {
            end++;
        }
        if (start == end || *end != '\0' || errno == ERANGE ||
            !isfinite(number) ||
            number < minimum || number > maximum) {
            printf("Enter a valid amount from %.2f to %.2f.\n",
                   minimum, maximum);
            continue;
        }
        *value = number;
        return 1;
    }
    return 0;
}

static int readRequiredText(const char *prompt, char *value, size_t size)
{
    int result;
    size_t length;
    char *start;

    while (!inputEnded) {
        result = readLine(prompt, value, size);
        if (result == 0) {
            return 0;
        }
        if (result < 0) {
            continue;
        }
        start = value;
        while (isspace((unsigned char)*start)) {
            start++;
        }
        length = strlen(start);
        while (length > 0 && isspace((unsigned char)start[length - 1])) {
            start[--length] = '\0';
        }
        if (length == 0) {
            printf("This field cannot be empty.\n");
            continue;
        }
        if (start != value) {
            memmove(value, start, length + 1);
        }
        return 1;
    }
    return 0;
}

static int readPhone(char *phone, size_t size)
{
    size_t i;
    size_t digits;

    while (!inputEnded) {
        if (!readRequiredText("Enter phone number (7-15 digits): ",
                              phone, size)) {
            return 0;
        }
        digits = strlen(phone);
        if (digits < 7 || digits > 15) {
            printf("Phone number must contain 7 to 15 digits.\n");
            continue;
        }
        for (i = 0; i < digits; i++) {
            if (!isdigit((unsigned char)phone[i])) {
                break;
            }
        }
        if (i != digits) {
            printf("Use digits only for the phone number.\n");
            continue;
        }
        return 1;
    }
    return 0;
}

static int isLeapYear(int year)
{
    return year % 400 == 0 || (year % 4 == 0 && year % 100 != 0);
}

static long dateOrdinal(int day, int month, int year)
{
    static const int daysBeforeMonth[] = {
        0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
    };
    long previousYears = year - 1L;
    long ordinal = previousYears * 365L + previousYears / 4L
                   - previousYears / 100L + previousYears / 400L;

    ordinal += daysBeforeMonth[month] + day;
    if (month > 2 && isLeapYear(year)) {
        ordinal++;
    }
    return ordinal;
}

static int parseDate(const char *text, Date *date)
{
    int monthDays[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    int i;

    if (strlen(text) != 10 || text[2] != '-' || text[5] != '-') {
        return 0;
    }
    for (i = 0; i < 10; i++) {
        if (i != 2 && i != 5 && !isdigit((unsigned char)text[i])) {
            return 0;
        }
    }

    date->day = (text[0] - '0') * 10 + text[1] - '0';
    date->month = (text[3] - '0') * 10 + text[4] - '0';
    date->year = (text[6] - '0') * 1000 + (text[7] - '0') * 100
                 + (text[8] - '0') * 10 + text[9] - '0';

    if (date->year < 1 || date->month < 1 || date->month > 12) {
        return 0;
    }
    if (isLeapYear(date->year)) {
        monthDays[1] = 29;
    }
    if (date->day < 1 || date->day > monthDays[date->month - 1]) {
        return 0;
    }
    date->ordinal = dateOrdinal(date->day, date->month, date->year);
    return 1;
}

static int readDate(const char *prompt, Date *date)
{
    char text[32];
    int result;

    while (!inputEnded) {
        result = readLine(prompt, text, sizeof(text));
        if (result == 0) {
            return 0;
        }
        if (result < 0) {
            continue;
        }
        if (parseDate(text, date)) {
            return 1;
        }
        printf("Enter a real date in DD-MM-YYYY format.\n");
    }
    return 0;
}

static void setDefaultRooms(void)
{
    static const Room defaults[] = {
        {101, "Standard", 1500.0, 1},
        {102, "Standard", 1500.0, 1},
        {201, "Deluxe",   2200.0, 1},
        {202, "Deluxe",   2200.0, 1},
        {301, "Suite",    3500.0, 1},
        {302, "Suite",    3500.0, 1}
    };

    memcpy(rooms, defaults, sizeof(defaults));
    roomCount = sizeof(defaults) / sizeof(defaults[0]);
}

static void loadCustomers(void)
{
    FILE *file;
    FileHeader header;

    file = fopen(CUSTOMER_FILE, "rb");
    if (file == NULL) {
        return;
    }
    if (fread(&header, sizeof(header), 1, file) != 1 ||
        memcmp(header.magic, "HTLCUS02", 8) != 0 ||
        header.version != DATA_VERSION || header.count > MAX_CUSTOMERS ||
        fread(customers, sizeof(Customer), header.count, file) != header.count) {
        printf("Warning: %s is invalid or incomplete; customer data was not loaded.\n",
               CUSTOMER_FILE);
        customerCount = 0;
    } else {
        customerCount = header.count;
    }
    if (fclose(file) != 0) {
        printf("Warning: could not close %s after reading.\n", CUSTOMER_FILE);
    }
}

static void loadBookings(void)
{
    FILE *file;
    FileHeader header;

    file = fopen(BOOKING_FILE, "rb");
    if (file == NULL) {
        return;
    }
    if (fread(&header, sizeof(header), 1, file) != 1 ||
        memcmp(header.magic, "HTLBOK02", 8) != 0 ||
        header.version != DATA_VERSION || header.count > MAX_BOOKINGS ||
        fread(bookings, sizeof(Booking), header.count, file) != header.count) {
        printf("Warning: %s is invalid or incomplete; booking data was not loaded.\n",
               BOOKING_FILE);
        bookingCount = 0;
    } else {
        bookingCount = header.count;
    }
    if (fclose(file) != 0) {
        printf("Warning: could not close %s after reading.\n", BOOKING_FILE);
    }
}

static void loadRooms(void)
{
    FILE *file;
    FileHeader header;

    file = fopen(ROOM_FILE, "rb");
    if (file == NULL) {
        return;
    }
    if (fread(&header, sizeof(header), 1, file) != 1 ||
        memcmp(header.magic, "HTLROM02", 8) != 0 ||
        header.version != DATA_VERSION || header.count == 0 ||
        header.count > MAX_ROOMS ||
        fread(rooms, sizeof(Room), header.count, file) != header.count) {
        printf("Warning: %s is invalid or incomplete; default rooms were loaded.\n",
               ROOM_FILE);
        setDefaultRooms();
    } else {
        roomCount = header.count;
    }
    if (fclose(file) != 0) {
        printf("Warning: could not close %s after reading.\n", ROOM_FILE);
    }
}

static void loadSettings(void)
{
    FILE *file;
    SettingsRecord settings;

    file = fopen(SETTINGS_FILE, "rb");
    if (file == NULL) {
        return;
    }
    if (fread(&settings, sizeof(settings), 1, file) == 1 &&
        memcmp(settings.magic, "HTLSET02", 8) == 0 &&
        settings.version == DATA_VERSION &&
        settings.cancellationFeePercent >= 0.0 &&
        settings.cancellationFeePercent <= 100.0) {
        cancellationFeePercent = settings.cancellationFeePercent;
    } else {
        printf("Warning: settings file is invalid; cancellation fee remains 0%%.\n");
    }
    if (fclose(file) != 0) {
        printf("Warning: could not close %s after reading.\n", SETTINGS_FILE);
    }
}

static void loadData(void)
{
    setDefaultRooms();
    customerCount = 0;
    bookingCount = 0;
    cancellationFeePercent = 0.0;
    loadCustomers();
    loadBookings();
    loadRooms();
    loadSettings();
}

static int saveCustomers(void)
{
    FILE *file = fopen(CUSTOMER_FILE, "wb");
    FileHeader header = {{'H','T','L','C','U','S','0','2'},
                         DATA_VERSION, (uint32_t)customerCount};
    int success = 1;

    if (file == NULL) {
        printf("Error: could not open %s for writing.\n", CUSTOMER_FILE);
        return 0;
    }
    if (fwrite(&header, sizeof(header), 1, file) != 1 ||
        fwrite(customers, sizeof(Customer), customerCount, file) != customerCount) {
        printf("Error: customer data could not be fully written.\n");
        success = 0;
    }
    if (fclose(file) != 0) {
        printf("Error: could not close %s after writing.\n", CUSTOMER_FILE);
        success = 0;
    }
    return success;
}

static int saveBookings(void)
{
    FILE *file = fopen(BOOKING_FILE, "wb");
    FileHeader header = {{'H','T','L','B','O','K','0','2'},
                         DATA_VERSION, (uint32_t)bookingCount};
    int success = 1;

    if (file == NULL) {
        printf("Error: could not open %s for writing.\n", BOOKING_FILE);
        return 0;
    }
    if (fwrite(&header, sizeof(header), 1, file) != 1 ||
        fwrite(bookings, sizeof(Booking), bookingCount, file) != bookingCount) {
        printf("Error: booking data could not be fully written.\n");
        success = 0;
    }
    if (fclose(file) != 0) {
        printf("Error: could not close %s after writing.\n", BOOKING_FILE);
        success = 0;
    }
    return success;
}

static int saveRooms(void)
{
    FILE *file = fopen(ROOM_FILE, "wb");
    FileHeader header = {{'H','T','L','R','O','M','0','2'},
                         DATA_VERSION, (uint32_t)roomCount};
    int success = 1;

    if (file == NULL) {
        printf("Error: could not open %s for writing.\n", ROOM_FILE);
        return 0;
    }
    if (fwrite(&header, sizeof(header), 1, file) != 1 ||
        fwrite(rooms, sizeof(Room), roomCount, file) != roomCount) {
        printf("Error: room data could not be fully written.\n");
        success = 0;
    }
    if (fclose(file) != 0) {
        printf("Error: could not close %s after writing.\n", ROOM_FILE);
        success = 0;
    }
    return success;
}

static int saveSettings(void)
{
    FILE *file = fopen(SETTINGS_FILE, "wb");
    SettingsRecord settings = {{'H','T','L','S','E','T','0','2'},
                               DATA_VERSION, cancellationFeePercent};
    int success = 1;

    if (file == NULL) {
        printf("Error: could not open %s for writing.\n", SETTINGS_FILE);
        return 0;
    }
    if (fwrite(&settings, sizeof(settings), 1, file) != 1) {
        printf("Error: settings could not be fully written.\n");
        success = 0;
    }
    if (fclose(file) != 0) {
        printf("Error: could not close %s after writing.\n", SETTINGS_FILE);
        success = 0;
    }
    return success;
}

static int findCustomer(int customerId)
{
    size_t i;

    for (i = 0; i < customerCount; i++) {
        if (customers[i].customerId == customerId) {
            return (int)i;
        }
    }
    return -1;
}

static int findRoom(int roomNumber)
{
    size_t i;

    for (i = 0; i < roomCount; i++) {
        if (rooms[i].roomNumber == roomNumber) {
            return (int)i;
        }
    }
    return -1;
}

static int findBooking(int bookingId)
{
    size_t i;

    for (i = 0; i < bookingCount; i++) {
        if (bookings[i].bookingId == bookingId) {
            return (int)i;
        }
    }
    return -1;
}

static int hasActiveBooking(int roomNumber)
{
    size_t i;

    for (i = 0; i < bookingCount; i++) {
        if (bookings[i].roomNumber == roomNumber &&
            bookings[i].status == STATUS_ACTIVE) {
            return 1;
        }
    }
    return 0;
}

/* Reservations use [check-in, check-out): departure day is not occupied. */
static int roomHasDateConflict(int roomNumber, const Date *checkIn,
                               const Date *checkOut)
{
    size_t i;
    Date existingIn;
    Date existingOut;

    for (i = 0; i < bookingCount; i++) {
        if (bookings[i].roomNumber != roomNumber ||
            bookings[i].status == STATUS_CANCELLED) {
            continue;
        }
        if (!parseDate(bookings[i].checkIn, &existingIn) ||
            !parseDate(bookings[i].status == STATUS_CHECKED_OUT
                           ? bookings[i].actualCheckOut
                           : bookings[i].checkOut,
                       &existingOut)) {
            return 1;
        }
        if (checkIn->ordinal < existingOut.ordinal &&
            existingIn.ordinal < checkOut->ordinal) {
            return 1;
        }
    }
    return 0;
}

static int nextCustomerId(void)
{
    int largest = 1000;
    size_t i;

    for (i = 0; i < customerCount; i++) {
        if (customers[i].customerId > largest) {
            largest = customers[i].customerId;
        }
    }
    return largest == INT_MAX ? 0 : largest + 1;
}

static int nextBookingId(void)
{
    int largest = 5000;
    size_t i;

    for (i = 0; i < bookingCount; i++) {
        if (bookings[i].bookingId > largest) {
            largest = bookings[i].bookingId;
        }
    }
    return largest == INT_MAX ? 0 : largest + 1;
}

static void displayRooms(int includeInactive)
{
    size_t i;
    const char *status;

    printf("\n---------------- ROOM DETAILS ----------------\n");
    printf("%-12s %-14s %-16s %-12s\n",
           "Room No.", "Category", "Rate/Night", "Status");
    for (i = 0; i < roomCount; i++) {
        if (!includeInactive && !rooms[i].active) {
            continue;
        }
        if (!rooms[i].active) {
            status = "Inactive";
        } else if (hasActiveBooking(rooms[i].roomNumber)) {
            status = "Reserved";
        } else {
            status = "Available";
        }
        printf("%-12d %-14s INR %-12.2f %-12s\n",
               rooms[i].roomNumber, rooms[i].category,
               rooms[i].ratePerNight, status);
    }
    printf("-----------------------------------------------\n");
}

static void registerCustomer(void)
{
    Customer customer;
    int id;

    if (customerCount >= MAX_CUSTOMERS) {
        printf("Customer storage is full (%d records).\n", MAX_CUSTOMERS);
        return;
    }
    id = nextCustomerId();
    if (id == 0) {
        printf("No further customer IDs are available.\n");
        return;
    }
    memset(&customer, 0, sizeof(customer));
    printf("\n------------- CUSTOMER REGISTRATION -------------\n");
    if (!readRequiredText("Enter customer name: ", customer.name,
                          sizeof(customer.name)) ||
        !readPhone(customer.phone, sizeof(customer.phone))) {
        return;
    }
    customer.customerId = id;
    customers[customerCount++] = customer;
    if (!saveCustomers()) {
        customerCount--;
        printf("Customer registration was not kept because saving failed.\n");
        return;
    }
    printf("Customer registered successfully. Customer ID: %d\n", id);
}

static void createBooking(void)
{
    Booking booking;
    Date checkIn;
    Date checkOut;
    int customerId;
    int customerIndex;
    int roomNumber;
    int roomIndex;

    if (bookingCount >= MAX_BOOKINGS) {
        printf("Booking storage is full (%d records).\n", MAX_BOOKINGS);
        return;
    }
    if (customerCount == 0) {
        printf("Register a customer before creating a booking.\n");
        return;
    }
    memset(&booking, 0, sizeof(booking));
    customerId = readInteger("Enter customer ID: ", 1, INT_MAX);
    if (customerId < 0) {
        return;
    }
    customerIndex = findCustomer(customerId);
    if (customerIndex < 0) {
        printf("Customer ID was not found.\n");
        return;
    }

    displayRooms(0);
    roomNumber = readInteger("Enter room number: ", 1, 999999);
    if (roomNumber < 0) {
        return;
    }
    roomIndex = findRoom(roomNumber);
    if (roomIndex < 0 || !rooms[roomIndex].active) {
        printf("That room does not exist or is inactive.\n");
        return;
    }

    if (!readDate("Check-in date (DD-MM-YYYY): ", &checkIn) ||
        !readDate("Check-out date (DD-MM-YYYY): ", &checkOut)) {
        return;
    }
    if (checkOut.ordinal <= checkIn.ordinal) {
        printf("Check-out must be later than check-in.\n");
        return;
    }
    if (roomHasDateConflict(roomNumber, &checkIn, &checkOut)) {
        printf("The room is already reserved for at least one of those nights.\n");
        return;
    }

    booking.bookingId = nextBookingId();
    if (booking.bookingId == 0) {
        printf("No further booking IDs are available.\n");
        return;
    }
    booking.customerId = customerId;
    booking.roomNumber = roomNumber;
    (void)snprintf(booking.roomCategory, sizeof(booking.roomCategory), "%s",
                   rooms[roomIndex].category);
    booking.ratePerNight = rooms[roomIndex].ratePerNight;
    (void)snprintf(booking.checkIn, sizeof(booking.checkIn), "%02d-%02d-%04d",
                   checkIn.day, checkIn.month, checkIn.year);
    (void)snprintf(booking.checkOut, sizeof(booking.checkOut), "%02d-%02d-%04d",
                   checkOut.day, checkOut.month, checkOut.year);
    booking.bookedNights = (int)(checkOut.ordinal - checkIn.ordinal);
    booking.mealDaysBooked = readInteger("Meal service days (0 to nights): ",
                                         0, booking.bookedNights);
    if (booking.mealDaysBooked < 0) {
        return;
    }
    booking.laundryItemsBooked = readInteger("Laundry items (0-1000): ", 0, 1000);
    if (booking.laundryItemsBooked < 0) {
        return;
    }
    booking.status = STATUS_ACTIVE;

    bookings[bookingCount++] = booking;
    if (!saveBookings()) {
        bookingCount--;
        printf("Booking was not kept because saving failed.\n");
        return;
    }
    printf("Booking created. Booking ID: %d | Customer: %s | Room: %d\n",
           booking.bookingId, customers[customerIndex].name, roomNumber);
    printf("Reserved %s through %s (%d nights).\n",
           booking.checkIn, booking.checkOut, booking.bookedNights);
}

static const char *bookingStatus(int status)
{
    if (status == STATUS_ACTIVE) {
        return "Active";
    }
    if (status == STATUS_CHECKED_OUT) {
        return "Checked out";
    }
    if (status == STATUS_CANCELLED) {
        return "Cancelled";
    }
    return "Unknown";
}

static void displayBookings(void)
{
    size_t i;

    printf("\n---------------- BOOKING DETAILS ----------------\n");
    if (bookingCount == 0) {
        printf("No booking records are available.\n");
        return;
    }
    printf("%-10s %-12s %-10s %-12s %-12s %-14s\n",
           "Book ID", "Customer", "Room", "Check-in", "Check-out", "Status");
    for (i = 0; i < bookingCount; i++) {
        printf("%-10d %-12d %-10d %-12s %-12s %-14s\n",
               bookings[i].bookingId, bookings[i].customerId,
               bookings[i].roomNumber, bookings[i].checkIn,
               bookings[i].checkOut, bookingStatus(bookings[i].status));
    }
}

static void searchCustomer(void)
{
    int id;
    int index;
    size_t i;
    int foundBooking = 0;

    id = readInteger("Enter customer ID to search: ", 1, INT_MAX);
    if (id < 0) {
        return;
    }
    index = findCustomer(id);
    if (index < 0) {
        printf("Customer ID %d was not found.\n", id);
        return;
    }
    printf("\nCustomer ID: %d\nName: %s\nPhone: %s\n",
           customers[index].customerId, customers[index].name,
           customers[index].phone);
    printf("Bookings:\n");
    for (i = 0; i < bookingCount; i++) {
        if (bookings[i].customerId == id) {
            printf("  Booking %d | Room %d | %s to %s | %s\n",
                   bookings[i].bookingId, bookings[i].roomNumber,
                   bookings[i].checkIn, bookings[i].checkOut,
                   bookingStatus(bookings[i].status));
            foundBooking = 1;
        }
    }
    if (!foundBooking) {
        printf("  No bookings found.\n");
    }
}

static void searchBooking(void)
{
    int id;
    int index;
    int customerIndex;

    id = readInteger("Enter booking ID to search: ", 1, INT_MAX);
    if (id < 0) {
        return;
    }
    index = findBooking(id);
    if (index < 0) {
        printf("Booking ID %d was not found.\n", id);
        return;
    }
    customerIndex = findCustomer(bookings[index].customerId);
    printf("\nBooking ID: %d\nCustomer ID: %d",
           bookings[index].bookingId, bookings[index].customerId);
    if (customerIndex >= 0) {
        printf(" (%s)", customers[customerIndex].name);
    }
    printf("\nRoom: %d\nCheck-in: %s\nReserved check-out: %s\nStatus: %s\n",
           bookings[index].roomNumber, bookings[index].checkIn,
           bookings[index].checkOut, bookingStatus(bookings[index].status));
    if (bookings[index].status == STATUS_CHECKED_OUT) {
        printf("Actual check-out: %s\n", bookings[index].actualCheckOut);
    } else if (bookings[index].status == STATUS_CANCELLED) {
        printf("Cancellation charge: INR %.2f\n",
               bookings[index].cancellationCharge);
    }
}

static int chooseCategory(char *category, size_t size)
{
    int choice;
    const char *categories[] = {"Standard", "Deluxe", "Suite"};

    printf("1. Standard (default INR 1500/night)\n");
    printf("2. Deluxe   (default INR 2200/night)\n");
    printf("3. Suite    (default INR 3500/night)\n");
    choice = readInteger("Choose category: ", 1, 3);
    if (choice < 0) {
        return 0;
    }
    (void)snprintf(category, size, "%s", categories[choice - 1]);
    return 1;
}

static void addRoom(void)
{
    Room room;
    int number;
    double defaultRate;

    if (roomCount >= MAX_ROOMS) {
        printf("Room storage is full (%d rooms).\n", MAX_ROOMS);
        return;
    }
    number = readInteger("Enter a new room number: ", 1, 999999);
    if (number < 0) {
        return;
    }
    if (findRoom(number) >= 0) {
        printf("That room number already exists.\n");
        return;    }
    memset(&room, 0, sizeof(room));
    if (!chooseCategory(room.category, sizeof(room.category))) {
        return;
    }
    if (strcmp(room.category, "Standard") == 0) {
        defaultRate = 1500.0;
    } else if (strcmp(room.category, "Deluxe") == 0) {
        defaultRate = 2200.0;
    } else {
        defaultRate = 3500.0;
    }
    room.roomNumber = number;
    room.active = 1;
    if (!readMoney("Enter rate per night (0 for category default): ",
                   0.0, 10000000.0,
                   &room.ratePerNight)) {
        return;
    }
    if (room.ratePerNight == 0.0) {
        room.ratePerNight = defaultRate;
    }
    rooms[roomCount++] = room;
    if (!saveRooms()) {
        roomCount--;
        printf("Room was not added because saving failed.\n");
        return;
    }
    printf("Room %d added as %s at INR %.2f per night.\n",
           room.roomNumber, room.category, room.ratePerNight);
}

static void editRoom(void)
{
    int number;
    int index;
    int editCategory;
    int editRate;
    double rate;
    char category[20];
    Room previous;

    displayRooms(1);
    number = readInteger("Enter room number to edit: ", 1, 999999);
    if (number < 0) {
        return;
    }
    index = findRoom(number);
    if (index < 0) {
        printf("Room was not found.\n");
        return;
    }
    editCategory = readInteger("Change category? (1=yes, 0=no): ", 0, 1);
    if (editCategory < 0) {
        return;
    }
    if (editCategory && !chooseCategory(category, sizeof(category))) {
        return;
    }
    editRate = readInteger("Change rate? (1=yes, 0=no): ", 0, 1);
    if (editRate < 0) {
        return;
    }
    if (editRate && !readMoney("Enter new rate per night: ", 0.01,
                               10000000.0, &rate)) {
        return;
    }
    previous = rooms[index];
    if (editCategory) {
        (void)snprintf(rooms[index].category, sizeof(rooms[index].category),
                       "%s", category);
    }
    if (editRate) {
        rooms[index].ratePerNight = rate;
    }
    if (!saveRooms()) {
        rooms[index] = previous;
        printf("Room changes were rolled back because saving failed.\n");
        return;
    }
    printf("Room %d updated.\n", number);
}

static void deactivateRoom(void)
{
    int number;
    int index;

    displayRooms(1);
    number = readInteger("Enter room number to deactivate: ", 1, 999999);
    if (number < 0) {
        return;
    }
    index = findRoom(number);
    if (index < 0 || !rooms[index].active) {
        printf("Active room was not found.\n");
        return;
    }
    if (hasActiveBooking(number)) {
        printf("Cannot deactivate a room with an active reservation.\n");
        return;
    }
    rooms[index].active = 0;
    if (!saveRooms()) {
        rooms[index].active = 1;
        printf("Room status was not changed because saving failed.\n");
        return;
    }
    printf("Room %d is now inactive.\n", number);
}

static void changeCancellationFee(void)
{
    double previous = cancellationFeePercent;
    double percentage;

    printf("Current cancellation fee: %.2f%%\n", cancellationFeePercent);
    if (!readMoney("Enter new cancellation fee percentage (0-100): ",
                   0.0, 100.0, &percentage)) {
        return;
    }
    cancellationFeePercent = percentage;
    if (!saveSettings()) {
        cancellationFeePercent = previous;
        printf("Setting was not changed because saving failed.\n");
        return;
    }
    printf("Cancellation fee set to %.2f%%.\n", cancellationFeePercent);
}

static void manageRooms(void)
{
    int choice;

    do {
        printf("\n--------------- ROOM ADMINISTRATION ---------------\n");
        printf("1. View all rooms\n");
        printf("2. Add a room\n");
        printf("3. Edit room category/rate\n");
        printf("4. Deactivate a room\n");
        printf("5. Change cancellation fee percentage\n");
        printf("6. Return to main menu\n");
        choice = readInteger("Choose an option (1-6): ", 1, 6);
        if (inputEnded) {
            return;
        }
        switch (choice) {
            case 1: displayRooms(1); break;
            case 2: addRoom(); break;
            case 3: editRoom(); break;
            case 4: deactivateRoom(); break;
            case 5: changeCancellationFee(); break;
            case 6: break;
        }
    } while (choice != 6 && !inputEnded);
}

static void cancelBooking(void)
{
    int id;
    int index;
    int roomIndex;
    double charge;

    id = readInteger("Enter active booking ID to cancel: ", 1, INT_MAX);
    if (id < 0) {
        return;
    }
    index = findBooking(id);
    if (index < 0 || bookings[index].status != STATUS_ACTIVE) {
        printf("An active booking with that ID was not found.\n");
        return;
    }
    roomIndex = findRoom(bookings[index].roomNumber);
    if (roomIndex < 0) {
        printf("Cancellation failed: the booking's room record is missing.\n");
        return;
    }
    charge = bookings[index].ratePerNight * bookings[index].bookedNights
             * cancellationFeePercent / 100.0;
    bookings[index].status = STATUS_CANCELLED;
    bookings[index].cancellationCharge = charge;
    if (!saveBookings()) {
        bookings[index].status = STATUS_ACTIVE;
        bookings[index].cancellationCharge = 0.0;
        printf("Cancellation was not kept because saving failed.\n");
        return;
    }
    printf("Booking %d cancelled. Cancellation charge (%.2f%% of reserved "
           "room charges): INR %.2f\n",
           id, cancellationFeePercent, charge);
    printf("The room is released for those dates.\n");
}

static void writeBill(FILE *stream, const Booking *booking)
{
    int customerIndex = findCustomer(booking->customerId);
    double roomCharge;
    double mealCharge;
    double laundryCharge;
    double subtotal;
    double discountAmount;
    double taxableAmount;
    double gstAmount;
    double finalBill;

    if (customerIndex < 0) {
        fprintf(stream, "Bill could not be prepared: related record missing.\n");
        return;
    }

    roomCharge = booking->ratePerNight * booking->actualNights;
    mealCharge = MEAL_RATE * booking->mealDaysUsed;
    laundryCharge = LAUNDRY_RATE * booking->laundryItemsUsed;
    subtotal = roomCharge + mealCharge + laundryCharge;
    discountAmount = subtotal * SAMPLE_DISCOUNT_PERCENT / 100.0;
    taxableAmount = subtotal - discountAmount;
    gstAmount = taxableAmount * SAMPLE_GST_PERCENT / 100.0;
    finalBill = taxableAmount + gstAmount;

    fprintf(stream, "\n============================================================\n");
    fprintf(stream, "             HOTEL ROOM BOOKING - FINAL BILL\n");
    fprintf(stream, "Hotel name: PPS Hotel (sample project)\n");
    fprintf(stream, "Customer name: %s\n", customers[customerIndex].name);
    fprintf(stream, "Customer ID: %d\n", customers[customerIndex].customerId);
    fprintf(stream, "Phone number: %s\n", customers[customerIndex].phone);
    fprintf(stream, "Booking ID: %d\n", booking->bookingId);
    fprintf(stream, "Room: %d (%s)\n",
            booking->roomNumber, booking->roomCategory);
    fprintf(stream, "Check-in date: %s\n", booking->checkIn);
    fprintf(stream, "Actual check-out date: %s\n", booking->actualCheckOut);
    fprintf(stream, "Nights stayed: %d\n", booking->actualNights);
    fprintf(stream, "------------------------------------------------------------\n");
    fprintf(stream, "Room charge (%d x INR %.2f): INR %.2f\n",
            booking->actualNights, booking->ratePerNight, roomCharge);
    fprintf(stream, "Meal charge (%d x INR %.2f): INR %.2f\n",
            booking->mealDaysUsed, MEAL_RATE, mealCharge);
    fprintf(stream, "Laundry charge (%d x INR %.2f): INR %.2f\n",
            booking->laundryItemsUsed, LAUNDRY_RATE, laundryCharge);
    fprintf(stream, "Subtotal: INR %.2f\n", subtotal);
    fprintf(stream, "Discount (%.2f%%): -INR %.2f\n",
            SAMPLE_DISCOUNT_PERCENT, discountAmount);
    fprintf(stream, "Taxable amount: INR %.2f\n", taxableAmount);
    fprintf(stream, "GST (sample %.2f%%): INR %.2f\n",
            SAMPLE_GST_PERCENT, gstAmount);
    fprintf(stream, "FINAL PAYABLE AMOUNT: INR %.2f\n", finalBill);
    fprintf(stream, "============================================================\n");
}

static int appendBillHistory(const Booking *booking)
{
    FILE *file = fopen(BILL_FILE, "a");
    int success = 1;

    if (file == NULL) {
        printf("Error: could not open %s for writing.\n", BILL_FILE);
        return 0;
    }
    writeBill(file, booking);
    if (ferror(file)) {
        printf("Error: bill history could not be fully written.\n");
        success = 0;
    }
    if (fclose(file) != 0) {
        printf("Error: could not close %s after writing.\n", BILL_FILE);
        success = 0;
    }
    return success;
}

static void checkoutCustomer(void)
{
    int id;
    int index;
    int mealDays;
    int laundryItems;
    Date actualDate;
    Date checkIn;
    Date reservedCheckOut;
    Booking previous;

    id = readInteger("Enter active booking ID for checkout: ", 1, INT_MAX);
    if (id < 0) {
        return;
    }
    index = findBooking(id);
    if (index < 0 || bookings[index].status != STATUS_ACTIVE) {
        printf("An active booking with that ID was not found.\n");
        return;
    }
    if (!readDate("Actual check-out date (DD-MM-YYYY): ", &actualDate)) {
        return;
    }
    if (!parseDate(bookings[index].checkIn, &checkIn) ||
        !parseDate(bookings[index].checkOut, &reservedCheckOut)) {
        printf("Checkout failed: saved reservation dates are invalid.\n");
        return;
    }
    if (actualDate.ordinal <= checkIn.ordinal ||
        actualDate.ordinal > reservedCheckOut.ordinal) {
        printf("Actual check-out must be after check-in and no later than "
               "the reserved check-out.\n");
        return;
    }
    mealDays = readInteger("Meal service days used (0 to actual nights): ",
                           0, (int)(actualDate.ordinal - checkIn.ordinal));
    if (mealDays < 0) {
        return;
    }
    if (mealDays > bookings[index].mealDaysBooked) {
        printf("Meal days used cannot exceed the number booked (%d).\n",
               bookings[index].mealDaysBooked);
        return;
    }
    laundryItems = readInteger("Laundry items used (0 to items booked): ",
                               0, bookings[index].laundryItemsBooked);
    if (laundryItems < 0) {
        return;
    }

    previous = bookings[index];
    (void)snprintf(bookings[index].actualCheckOut,
                   sizeof(bookings[index].actualCheckOut),
                   "%02d-%02d-%04d",
                   actualDate.day, actualDate.month, actualDate.year);
    bookings[index].actualNights = (int)(actualDate.ordinal - checkIn.ordinal);
    bookings[index].mealDaysUsed = mealDays;
    bookings[index].laundryItemsUsed = laundryItems;
    bookings[index].status = STATUS_CHECKED_OUT;
    if (!saveBookings()) {
        bookings[index] = previous;
        printf("Checkout was not kept because saving failed.\n");
        return;
    }
    writeBill(stdout, &bookings[index]);
    if (appendBillHistory(&bookings[index])) {
        printf("Bill copy saved in %s.\n", BILL_FILE);
    }
    printf("Checkout completed. Room %d is released from %s onward.\n",
           bookings[index].roomNumber, bookings[index].actualCheckOut);
}

static void generateFinalBill(void)
{
    int id;
    int index;

    id = readInteger("Enter checked-out booking ID: ", 1, INT_MAX);
    if (id < 0) {
        return;
    }
    index = findBooking(id);
    if (index < 0 || bookings[index].status != STATUS_CHECKED_OUT) {
        printf("A checked-out booking with that ID was not found.\n");
        return;
    }
    writeBill(stdout, &bookings[index]);
    if (appendBillHistory(&bookings[index])) {
        printf("Bill copy saved in %s.\n", BILL_FILE);
    }
}

static void viewBillHistory(void)
{
    FILE *file = fopen(BILL_FILE, "r");
    char line[256];
    int readError = 0;

    if (file == NULL) {
        printf("No bill history is available yet (%s).\n", BILL_FILE);
        return;
    }
    printf("\n---------------- BILL HISTORY ----------------\n");
    while (fgets(line, sizeof(line), file) != NULL) {
        fputs(line, stdout);
    }
    if (ferror(file)) {
        printf("Error: bill history could not be fully read.\n");
        readError = 1;
    }
    if (fclose(file) != 0) {
        printf("Error: could not close %s after reading.\n", BILL_FILE);
        readError = 1;
    }
    if (!readError) {
        printf("\n-------------- END OF BILL HISTORY --------------\n");
    }
}

static void showMenu(void)
{
    printf("\n====================================================\n");
    printf("HOTEL ROOM BOOKING AND CUSTOMER MANAGEMENT SYSTEM\n");
    printf("====================================================\n");
    printf("1. Display Rooms\n");
    printf("2. Register Customer\n");
    printf("3. Manage Rooms\n");
    printf("4. Create New Booking\n");
    printf("5. Display All Bookings\n");
    printf("6. Search Customer\n");
    printf("7. Search Booking\n");
    printf("8. Cancel Booking\n");
    printf("9. Customer Checkout\n");
    printf("10. Generate Final Bill\n");
    printf("11. View Bill History\n");
    printf("12. Exit\n");
}

int main(void)
{
    int choice;

    loadData();
    printf("Welcome. This is a single-user, console-based hotel system.\n");

    while (!inputEnded) {
        showMenu();
        choice = readInteger("Enter your choice (1-12): ", 1, 12);
        if (inputEnded) {
            break;
        }
        switch (choice) {
            case 1: displayRooms(0); break;
            case 2: registerCustomer(); break;
            case 3: manageRooms(); break;
            case 4: createBooking(); break;
            case 5: displayBookings(); break;
            case 6: searchCustomer(); break;
            case 7: searchBooking(); break;
            case 8: cancelBooking(); break;
            case 9: checkoutCustomer(); break;
            case 10: generateFinalBill(); break;
            case 11: viewBillHistory(); break;
            case 12:
                printf("Goodbye.\n");
                return 0;
        }
    }
    printf("\nInput ended. Exiting without waiting for more input.\n");
    return 0;
}