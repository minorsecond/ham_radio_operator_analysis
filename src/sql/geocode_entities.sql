select *
from ham_ops.geocoded_entity_data
order by id desc;

ALTER TABLE ham_ops.geocoded_entity_data
ADD COLUMN id SERIAL;

SELECT 
	(COUNT(*) FILTER (WHERE rating >= 50)) AS count_below_threshold,
    (COUNT(*) FILTER (WHERE rating >= 50) * 100.0 / COUNT(*)) AS percentage_below_threshold,
    (SUM(rating) / COUNT(*)) AS average_rating
FROM 
    ham_ops.geocoded_entity_data;
    
select count(*)
from ham_ops.entity_data

                select *
from ham_ops.bad_geocode

SELECT *
FROM ham_ops.entity_data
WHERE street_address LIKE '%APO%';

SELECT
    rating_category,
    COUNT(*) AS total_records,
    CASE 
        WHEN (SELECT COUNT(*) FROM ham_ops.geocoded_entity_data) = 0 THEN 0
        ELSE ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM ham_ops.geocoded_entity_data), 2)
    END AS percentage
FROM (
    SELECT
        CASE
            WHEN rating <= 10 THEN 'Excellent'
            WHEN rating <= 30 THEN 'Good'
            WHEN rating <= 50 THEN 'Fair'
            ELSE 'Poor'
        END AS rating_category
    FROM
        ham_ops.geocoded_entity_data
) AS categories
GROUP BY
    rating_category
ORDER BY
    rating_category;
