SELECT ${f(join_cols, "seats")}, CEIL(AVG(seats.seats)) AS number_of_seats
FROM gros.seats
GROUP BY ${f(join_cols, "seats")}
