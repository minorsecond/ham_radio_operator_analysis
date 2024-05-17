-- Indices for the amateur_data table
CREATE INDEX idx_amateur_data_usi ON amateur_data (unique_system_identifier);

-- Indices for the entity_data table
CREATE INDEX idx_entity_data_usi ON entity_data (unique_system_identifier);
CREATE INDEX idx_entity_data_geocode ON entity_data (street_address, city, state, zip_code) 
WHERE po_box IS NULL;
CREATE INDEX idx_entity_data_address ON entity_data (street_address, city, state, zip_code);
