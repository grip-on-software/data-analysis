SELECT seats.project_id, seats.sprint_id, CEIL(AVG(seats.seats)) AS number_of_seats
FROM gros.seats
GROUP BY seats.project_id, seats.sprint_id
