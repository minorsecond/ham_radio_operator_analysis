import csv
import psycopg2
from datetime import datetime

def convert_date(date_str):
    try:
        return datetime.strptime(date_str, '%m/%d/%Y').strftime('%Y-%m-%d')
    except ValueError:
        return None

# Define database parameters and connect
params = {
    "host": "192.168.3.231",
    "database": "ross_projects",
    "user": "rwardrup",
    "password": "Rward0232"
}
conn = psycopg2.connect(**params)
cursor = conn.cursor()

# Create or update the table structure
cursor.execute("""
    CREATE TABLE IF NOT EXISTS ham_ops.license_status (
        unique_system_identifier INT PRIMARY KEY,
        license_status CHAR(1),
        effective_date DATE,
        last_action_date DATE
    );
""")
conn.commit()
print("Table 'ham_ops.license_status' created or updated successfully.")

# Read and insert data from the HD.dat file
with open(r"C:\Users\rossw\Downloads\l_amat\HD.dat", 'r', newline='', encoding='utf-8') as file:
    reader = csv.reader(file, delimiter='|')
    next(reader)  # Skip header if present
    count = 0
    for row in reader:
        unique_system_identifier = int(row[1])  # Assuming Unique System Identifier is the second column
        license_status = row[5]  # Assuming License Status is the sixth column
        effective_date = convert_date(row[42])  # Assuming Effective Date is the 43rd column
        last_action_date = convert_date(row[43])  # Assuming Last Action Date is the 44th column

        # Prepare and execute the insert query
        cursor.execute(
            """
            INSERT INTO ham_ops.license_status 
            (unique_system_identifier, license_status, effective_date, last_action_date) 
            VALUES (%s, %s, %s, %s)
            ON CONFLICT (unique_system_identifier) DO UPDATE SET 
            license_status = EXCLUDED.license_status,
            effective_date = EXCLUDED.effective_date,
            last_action_date = EXCLUDED.last_action_date;
            """,
            (unique_system_identifier, license_status, effective_date, last_action_date)
        )
        count += 1
        if count % 1000 == 0:
            conn.commit()  # Commit every 1000 records
            print(f"Processed {count} records...")

# Final commit and cleanup
conn.commit()
cursor.close()
conn.close()
print(f"Data upload completed successfully, total records processed: {count}.")
