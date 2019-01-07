-- Sprint sequence number within project
SELECT ${f(join_cols, "sprint")}, COUNT(*) AS sprint_num
FROM (gros.${t("sprint")} AS sprint1
	JOIN gros.project ON sprint1.project_id = project.project_id
), gros.${t("sprint")} AS sprint2
WHERE sprint1.project_id = sprint2.project_id
--AND ${s(sprint_open)} >= sprint2.start_date
AND sprint1.sprint_id >= sprint2.sprint_id
${s(sprint_conditions, sprint='sprint1')}
${s(sprint_conditions, sprint='sprint2')}
GROUP BY ${f(join_cols, "sprint")}
