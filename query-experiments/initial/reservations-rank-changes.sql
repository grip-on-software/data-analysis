SELECT DISTINCT issue.project_id, issue.updated_by, reservation.requester, reservation.description FROM gros.issue
 JOIN gros.reservation
ON issue.project_id = reservation.project_id
AND issue.updated BETWEEN reservation.start_date AND reservation.end_date
WHERE issue.rank_change IS NOT NULL;
