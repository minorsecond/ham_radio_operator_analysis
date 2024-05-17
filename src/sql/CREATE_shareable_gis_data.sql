CREATE OR REPLACE VIEW ham_ops.shareable_geocoded_hams AS
SELECT 
    ed.id,
    ed.latitude,
    ed.longitude,
    ed.rating AS tiger_rating,
    ls.license_status,
    ls.effective_date
    ST_AsText(ed.geom) AS geom_wkt
FROM 
    ham_ops.geocoded_entity_data ed
LEFT JOIN ham_ops.license_status ls ON ed.unique_system_identifier = ls.unique_system_identifier;