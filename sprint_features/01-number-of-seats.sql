SELECT ${f(join_cols, "seats")}, CEIL(AVG(seats.seats)) AS number_of_seats
FROM gros.seats
${g(join_cols, "seats")}
