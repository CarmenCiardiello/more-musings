// Quarto pre-render script (runs with Quarto's bundled Deno, no R needed).
//
// The shinylive R package rebuilds .quarto/_webr/appdir on every render and
// deletes the old copy with fs::dir_delete(), which fails on this machine
// (x64 R under emulation inside a OneDrive folder) with
// "[EPERM] Failed to remove '.quarto/_webr/appdir'". Clearing the folder
// here first lets the Batted Ball Trajectory page render reliably.
for (const dir of [".quarto/_webr/appdir"]) {
  try {
    Deno.removeSync(dir, { recursive: true });
  } catch (_err) {
    // Nothing to remove, or already gone.
  }
}
