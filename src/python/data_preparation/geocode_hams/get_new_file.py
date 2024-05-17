import os
import json
import requests
import zipfile
import io
import csv
from sqlalchemy import create_engine, MetaData, Table, Column, Integer, String
from sqlalchemy.orm import sessionmaker

def read_config(filename="config.json"):
    try:
        with open(filename, "r") as f:
            return json.load(f)
    except FileNotFoundError:
        print("Config file not found.")
        return None

# Read database configuration from config.json
config = read_config()
if not config:
    exit()

# Extract database configuration
database_config = config.get("database")

# Construct database URL
DATABASE_URL = f"postgresql://{database_config['user']}:{database_config['password']}@{database_config['host']}:{database_config['port']}/{database_config['dbname']}"

# Define database connection
engine = create_engine(DATABASE_URL)
Session = sessionmaker(bind=engine)
session = Session()

# Define SQLAlchemy metadata
metadata = MetaData()

# Define the entity_data table
entity_data = Table(
    "entity_data",
    metadata,
    Column("record_type", String),
    Column("unique_system_identifier", Integer, primary_key=True),
    Column("uls_file_number", String),
    Column("ebf_number", String),
    Column("call_sign", String),
    Column("entity_type", String),
    Column("licensee_id", String),
    Column("entity_name", String),
    Column("first_name", String),
    Column("mi", String),
    Column("last_name", String),
    Column("suffix", String),
    Column("phone", String),
    Column("fax", String),
    Column("email", String),
    Column("street_address", String),
    Column("city", String),
    Column("state", String),
    Column("zip_code", String),
    Column("po_box", String),
    Column("attention_line", String),
    Column("sgin", String),
    Column("fcc_registration_number", String),
    Column("applicant_type_code", String),
    Column("applicant_type_code_other", String),
    Column("status_code", String),
    Column("status_date", String),
    Column("three_point_seven_ghz_license_type", String),
    Column("linked_unique_system_identifier", Integer),
    Column("linked_call_sign", String),
)

# Download and extract the ZIP file
url = "https://data.fcc.gov/download/pub/uls/complete//l_amat.zip"
response = requests.get(url)
zip_file = zipfile.ZipFile(io.BytesIO(response.content))
zip_file.extractall()

# Read the extracted EN.dat file
en_dat_file = os.path.join(zip_file.namelist()[0], "EN.dat")
with open(en_dat_file, "r") as file:
    reader = csv.reader(file, delimiter="|")
    for row in reader:
        record_type = row[0]
        unique_system_identifier = int(row[1])
        uls_file_number = row[2]
        ebf_number = row[3]
        call_sign = row[4]
        entity_type = row[5]
        licensee_id = row[6]
        entity_name = row[7]
        first_name = row[8]
        mi = row[9]
        last_name = row[10]
        suffix = row[11]
        phone = row[12]
        fax = row[13]
        email = row[14]
        street_address = row[15]
        city = row[16]
        state = row[17]
        zip_code = row[18]
        po_box = row[19]
        attention_line = row[20]
        sgin = row[21]
        fcc_registration_number = row[22]
        applicant_type_code = row[23]
        applicant_type_code_other = row[24]
        status_code = row[25]
        status_date = row[26]
        three_point_seven_ghz_license_type = row[27]
        linked_unique_system_identifier = int(row[28])
        linked_call_sign = row[29]

        # Check if the record already exists in the database
        existing_record = (
            session.query(entity_data)
            .filter_by(unique_system_identifier=unique_system_identifier)
            .first()
        )

        # If the record doesn't exist or any of the fields have changed, update the database
        if not existing_record or any(
            getattr(existing_record, column) != locals()[column]
            for column in entity_data.columns.keys()
        ):
            # Update or insert the record
            if existing_record:
                for column in entity_data.columns.keys():
                    setattr(existing_record, column, locals()[column])
            else:
                new_record = entity_data.insert().values(
                    **{
                        column: locals()[column]
                        for column in entity_data.columns.keys()
                        if column != "unique_system_identifier"
                    }
                )
                session.execute(new_record)

# Commit the changes to the database
session.commit()

# Close the database session
session.close()

# Clean up: Remove the downloaded ZIP file and extracted EN.dat file
os.remove(en_dat_file)
zip_file.close()
