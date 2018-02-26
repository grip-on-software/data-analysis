SELECT project_id, sprint_id, CAST(${sprint_closed} AS INT) AS sprint_is_closed FROM gros.sprint;
