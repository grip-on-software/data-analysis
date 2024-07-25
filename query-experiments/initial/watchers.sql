SELECT project_id, watchers, COUNT(*) FROM gros.issue WHERE status=6 GROUP BY project_id, watchers ORDER BY project_id, watchers;

-- Replace watchers with story_points for more stats
