SELECT project_id, COUNT(DISTINCT issue_id) AS num_tests FROM gros.issue
WHERE ${issue_test_case} GROUP BY project_id;
