import json
import time
import numpy as np
import pandas as pd
from sqlalchemy import create_engine, Column, Integer, Float, String, text
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from geoalchemy2 import Geometry, WKTElement
import usaddress
import pgeocode

VERBOSE = True

def reconstruct_standardized_address(parsed_address):
    reconstructed_address = ''
    for component, label in parsed_address:
        if label in ['BuildingName', 'StreetName']:
            # Capitalize the first letter of each word in these components
            component = component.title()
        reconstructed_address += component + ' '
    return reconstructed_address.strip()

def standardize_address(address):
    try:
        parsed_address = usaddress.tag(address)
        new_address = reconstruct_standardized_address(parsed_address)
        return new_address
    except Exception:
        return address
def read_config(filename="config.json"):
    try:
        with open(filename, "r") as f:
            return json.load(f)
    except FileNotFoundError:
        print("Config file not found.")
        return None

Base = declarative_base()

class BadGeocode(Base):
    __tablename__ = 'bad_geocode'
    __table_args__ = {'schema': 'ham_ops'}

    id = Column(Integer, primary_key=True)
    unique_system_identifier = Column(Integer)

class EntityData(Base):
    __tablename__ = 'entity_data'
    __table_args__ = {'schema': 'ham_ops'}

    unique_system_identifier = Column(Integer, primary_key=True)
    street_address = Column(String)
    city = Column(String)
    state = Column(String)
    zip_code = Column(String)
    po_box = Column(String)

class GeocodedEntityData(Base):
    __tablename__ = 'geocoded_entity_data'
    __table_args__ = {'schema': 'ham_ops'}

    unique_system_identifier = Column(Integer, primary_key=True)
    latitude = Column(Float)
    longitude = Column(Float)
    rating = Column(Integer)
    geom = Column(Geometry('POINT', srid=4326))

def connect_to_db(config):
    try:
        engine = create_engine(
            f"postgresql+psycopg2://{config['database']['user']}:{config['database']['password']}@{config['database']['host']}:{config['database']['port']}/{config['database']['dbname']}?client_encoding=utf8"
        )
        Base.metadata.create_all(engine)
        Session = sessionmaker(bind=engine)
        return Session()
    except Exception as e:
        print(f"Error connecting to PostgreSQL database: {e}")

def fetch_ungeocoded_records(session, state):
    try:
        bad_geocode_subquery = session.query(BadGeocode.unique_system_identifier).filter(
            BadGeocode.unique_system_identifier.isnot(None))
        geocoded_subquery = session.query(GeocodedEntityData.unique_system_identifier).filter(
            GeocodedEntityData.unique_system_identifier.isnot(None))

        # Query entity_data table, excluding records with unique identifiers present in bad_geocode
        # or those already geocoded in geocoded_entity_data table for the given state
        query = session.query(EntityData).filter(
            EntityData.state == state,
            ~EntityData.unique_system_identifier.in_(bad_geocode_subquery),
            ~EntityData.unique_system_identifier.in_(geocoded_subquery)
        ).order_by(EntityData.state, EntityData.city)

        return query.all()
    except Exception as e:
        print(f"Error fetching un-geocoded records for {state}: {e}")
        return []

def geocode_zip_code(zip_code):
    try:
        nomi = pgeocode.Nominatim('us')
        query = nomi.query_postal_code(str(zip_code).strip())

        # Ensure latitude and longitude are numeric
        query['latitude'] = pd.to_numeric(query['latitude'], errors='coerce')
        query['longitude'] = pd.to_numeric(query['longitude'], errors='coerce')

        # Check if the geocoding result is valid and contains non-NaN values
        if query is not None and not query[['latitude', 'longitude']].isna().any(axis=None):
            rating = query["accuracy"]
            # Set rating to None if it's NaN
            if pd.isna(rating):
                rating = None
            return {"rating": rating, "lat": query["latitude"], "lon": query["longitude"]}
        else:
            if VERBOSE:
                print(f"    - Invalid geocoding result for ZIP code: {zip_code}")
            return None
    except Exception as e:
        print(e)
        return None

def geocode_city(city, state):
    try:
        nomi = pgeocode.Nominatim('us')
        loc_string = f'{city.strip()}, {state.strip()}'
        query = nomi.query_location(loc_string)

        # Ensure latitude and longitude are numeric
        query['latitude'] = pd.to_numeric(query['latitude'], errors='coerce')
        query['longitude'] = pd.to_numeric(query['longitude'], errors='coerce')

        # Check if the geocoding result is valid and contains non-NaN values
        if query is not None and not query[['latitude', 'longitude']].isna().any(axis=None):
            rating = query["accuracy"]
            # Set rating to None if it's NaN
            if pd.isna(rating):
                rating = None
            return {"rating": rating, "lat": query["latitude"], "lon": query["longitude"]}
        else:
            if VERBOSE:
                print(f"    - Invalid geocoding result for location: {loc_string}")
            return None
    except Exception as e:
        print(e)
        return None

def update_bad_geocodes(uid, session):
    try:
        bad_geocode_record = BadGeocode(unique_system_identifier=uid)
        session.add(bad_geocode_record)
        session.commit()
    except Exception as e:
        print(f"Error storing record in bad_geocode table: {e}")
        session.rollback()

def geocode_address(session, address, city, state, zip_code, uid):
    try:
        if VERBOSE:
            print(f"  - Attempting to geocode {address}")
        address = standardize_address(address)
        if VERBOSE:
            print(f"   - Standardized Address: {address}")
        query = text("SELECT rating, ST_X(geomout) AS lon, ST_Y(geomout) AS lat FROM geocode(:address, 1) AS g;")
        result = session.execute(query, {"address": address}).fetchone()
        if result:
            if VERBOSE:
                print(f"    - Success geocoding: rating: {result[0]}")
            return {"rating": result[0], "longitude": result[1], "latitude": result[2]}
        else:
            if VERBOSE:
                print(f"    - Full address geocoding failed, attempting ZIP code-level geocoding for {zip_code}, {state}")
            zip_level_address = f"{zip_code}"
            result = geocode_zip_code(zip_level_address)
            if result:
                if VERBOSE:
                    print(f"    - Success ZIP code-level geocoding")
                return {"rating": 5000 + (6 - (result['rating'] if result['rating'] is not None else 0)),
                        "longitude": result['lon'], "latitude": result['lat']}
            else:
                if VERBOSE:
                    print(f"    - ZIP code-level geocoding failed, attempting city-level geocoding for {city}, {state}")
                result = geocode_city(city, state)
                if result:
                    if VERBOSE:
                        print(f"    - Success city-level geocoding")
                    return {"rating": 5000 + (6 - (result['rating'] if result['rating'] is not None else 0)),
                            "longitude": result['lon'], "latitude": result['lat']}
                else:
                    try:
                        bad_geocode_record = BadGeocode(unique_system_identifier=uid)
                        session.add(bad_geocode_record)
                        session.commit()
                    except Exception as e:
                        print(f"Error storing record in bad_geocode table: {e}")
                        session.rollback()
                    return None
    except Exception as e:
        session.rollback()
        print(f"Error geocoding address {address}: {e}")
        update_bad_geocodes(uid, session)

    return None

def update_geocoded_data(session, data):
    try:
        point_wkt = f"POINT({data['longitude']} {data['latitude']})"
        point_geom = WKTElement(point_wkt, srid=4326)
        geocoded_entity = GeocodedEntityData(
            unique_system_identifier=data["unique_system_identifier"],
            latitude=data["latitude"],
            longitude=data["longitude"],
            rating=data["rating"],
            geom=point_geom
        )
        session.add(geocoded_entity)
        session.commit()
    except Exception as e:
        session.rollback()
        print(f"Error updating geocoded_entity_data table: {e}")

def format_time(seconds):
    intervals = (
        ('weeks', 604800),
        ('days', 86400),
        ('hours', 3600),
        ('minutes', 60),
        ('seconds', 1),
    )
    result = []
    for name, count in intervals:
        value = seconds // count
        if value:
            seconds -= value * count
            if value == 1:
                name = name.rstrip('s')
            result.append(f"{int(value)} {name}")
    return ', '.join(result)

def main():
    config = read_config()
    if not config:
        return
    session = connect_to_db(config)
    limit = 1000
    start_time = time.time()
    already_processed = session.query(GeocodedEntityData).count()
    total_records_processed = already_processed
    current_processed = 0
    to_process = session.query(EntityData).count()
    print(f"Geocoding {to_process - already_processed} records")
    unique_states = session.query(EntityData.state).distinct().all()
    unique_states = [state[0] for state in unique_states]

    for state in unique_states:
        print(f"Geocoding records for state: {state}")
        records = fetch_ungeocoded_records(session, state)

        for record in records:
            if not any([record.street_address, record.city,record.state, record.zip_code]):
                update_bad_geocodes(record.unique_system_identifier, session)
                if VERBOSE:
                    print("  - All address components are empty or None. Skipping geocoding.")
            else:
                address = f"{record.street_address}, {record.city} {record.state} {record.zip_code}"
                geocode_result = geocode_address(session, address, record.city, record.state, record.zip_code, record.unique_system_identifier)
                if geocode_result:
                    update_geocoded_data(session, {
                        "unique_system_identifier": record.unique_system_identifier,
                        "latitude": geocode_result["latitude"],
                        "longitude": geocode_result["longitude"],
                        "rating": geocode_result["rating"]
                    })
                total_records_processed += 1
                current_processed += 1
                if total_records_processed % 100 == 0:
                    elapsed_time = time.time() - start_time
                    percentage_complete = total_records_processed / (to_process - already_processed) * 100
                    time_remaining = (elapsed_time / current_processed) * (
                                to_process - already_processed - current_processed)
                    print(
                        f"Geocoding State: {state} | "
                        f"Progress: {total_records_processed}/{to_process} ({percentage_complete:.2f}%) | "
                        f"Elapsed Time: {format_time(elapsed_time)} | "
                        f"Estimated Time Remaining: {format_time(time_remaining)}")

    end_time = time.time()
    print(f"Total time taken: {end_time - start_time} seconds")

if __name__ == "__main__":
    main()
