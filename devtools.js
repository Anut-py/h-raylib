#!/usr/bin/env node
const {
  rmSync,
  readFileSync,
  existsSync,
  writeFileSync,
  copyFileSync,
} = require("fs");
const { execSync, spawnSync } = require("child_process");
const path = require("path");
const readline = require("readline");

const exec = (...c) => execSync(...c).toString();

const silent = process.argv.includes("-s") || process.argv.includes("--silent"); // No logs
const quiet = process.argv.includes("-q") || process.argv.includes("--quiet"); // Errors only
const verbose =
  process.argv.includes("-v") || process.argv.includes("--verbose"); // All logs
const normal = !silent && !quiet && !verbose; // Non-verbose logs

const hraylibVersion = readFileSync(path.join(__dirname, "h-raylib.cabal"))
  .toString()
  .split("\n")
  .find((line) => line.startsWith("version:"))
  .split(/\s+/)[1];

const logLevel = { VERB: 0, INFO: 1, WARN: 2, ERR: 3 };
const log = (level, ...strs) => {
  // Level 0 = verbose, level 1 = info, level 2 = warning, level 3 = error
  if (silent) return;
  if (level === logLevel.ERR) console.error("(error  )", ...strs);
  if (quiet) return;
  if (level === logLevel.WARN) console.warn("(warning)", ...strs);
  if (level === logLevel.INFO) console.log("(info   )", ...strs);
  if (normal) return;
  if (level === logLevel.VERB) console.log("(verbose)", ...strs);
};

const withQuotes = (str) => (str.includes(" ") ? `"${str}"` : str);
const findCommandPathOnPath = (commandName) => {
  let fromPath = undefined;
  outer: for (let dir of process.env.PATH?.split(path.delimiter) ?? []) {
    for (let ext of ["", ".exe", ".cmd", ".bat", ".sh"]) {
      const file = path.join(dir, commandName + ext);
      if (existsSync(file)) {
        fromPath = withQuotes(file);
        break outer;
      }
    }
  }
  return fromPath;
};
const findCommandPath = (commandName, longForm, shortForm) => {
  log(logLevel.VERB, `Looking for ${commandName} path on command line`);
  if (longForm !== undefined && longForm !== null) {
    const longPath = process.argv.find((arg) => arg.startsWith(longForm + "="));
    if (longPath !== undefined) {
      log(logLevel.VERB, `${commandName} path found on command line!`);
      return withQuotes(np.substring((longForm + "=").length));
    }
  }
  if (shortForm !== undefined && shortForm !== null) {
    const idx = process.argv.indexOf(shortForm);
    if (idx !== -1) {
      if (process.argv.length >= idx + 2) {
        log(logLevel.VERB, `${commandName} path found on command line!`);
        return withQuotes(process.argv[idx + 1]);
      } else {
        log(
          logLevel.ERR,
          `${shortForm} was passed but no ${commandName} path was provided!`
        );
        process.exit(1);
      }
    }
  }
  log(logLevel.VERB, `${commandName} path not found on command line`);
  log(logLevel.VERB, `Looking for ${commandName} in PATH environment variable`);

  let fromPath = findCommandPathOnPath(commandName);
  if (fromPath !== undefined) {
    log(logLevel.VERB, `${commandName} path found in PATH!`);
    return fromPath;
  }

  log(logLevel.WARN, `No ${commandName} path was found`);
  log(logLevel.WARN, `Using fallback \`${commandName}\``);

  return commandName;
};
const diffWithRemote = (module) => {
  const modulePath = withQuotes(path.join(__dirname, module));
  const gitPath = findCommandPath("git", "--git-path", null);

  log(logLevel.VERB, "Using git path: " + gitPath);
  log(logLevel.VERB, "Running `git fetch`");

  exec(`cd ${modulePath} && ${gitPath} fetch`);

  log(logLevel.VERB, "Comparing revisions");

  const headRev = exec(`cd ${modulePath} && ${gitPath} rev-parse HEAD`).split("\n")[0];
  const remoteRev = exec(`cd ${modulePath} && ${gitPath} rev-parse origin/master`).split(
    "\n"
  )[0];

  log(logLevel.VERB, "Local revision:  " + headRev);
  log(logLevel.VERB, "Remote revision: " + remoteRev);

  if (headRev === remoteRev) {
    log(logLevel.INFO, "Already up to date");
  } else {
    spawnSync(`cd ${modulePath} && ${gitPath} diff HEAD origin/master`, {
      stdio: "inherit",
      shell: true,
    });

    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
    });

    log(logLevel.VERB, "Prompting to pull");

    rl.setPrompt("Pull code (y/N)? ");
    rl.prompt(true);

    rl.on("line", (line) => {
      log(logLevel.VERB, "Prompt response: " + line);
      if (line.trim().toLowerCase() === "y") {
        log(logLevel.INFO, "Pulling code");

        exec(`cd ${modulePath} && ${gitPath} pull`);
        
        log(logLevel.INFO, "Don't forget to run nix-update")
      }
      log(logLevel.INFO, "All done!");
      process.exitCode = 0;
      rl.close();
    });
  }
}

if (process.argv.includes("-u") || process.argv.includes("--nix-update")) {
  (() => {
    const nixPath = findCommandPath("nix", "--nix-path", "-n");
    const gitPath = findCommandPath("git", "--git-path", null);

    log(logLevel.INFO, "Using nix path: " + nixPath);
    log(logLevel.INFO, "Using git path: " + gitPath);

    log(logLevel.VERB, "Fetching git revisions");

    const raylibGitOutput = exec(`cd raylib && ${gitPath} rev-parse HEAD`);
    const raylibRevision = raylibGitOutput.split("\n")[0];

    log(logLevel.VERB, "Found raylib revision: " + raylibRevision);

    const rayguiGitOutput = exec(`cd raygui && ${gitPath} rev-parse HEAD`);
    const rayguiRevision = rayguiGitOutput.split("\n")[0];

    log(logLevel.VERB, "Found raygui revision: " + rayguiRevision);

    log(logLevel.INFO, "h-raylib version " + hraylibVersion);

    const flakeNixRaw = readFileSync(
      path.join(__dirname, "flake.nix")
    );

    const flakeUpdatedRaylib = flakeNixRaw.includes(`raylibRev = "${raylibRevision}"`);
    const flakeUpdatedRaygui = flakeNixRaw.includes(`rayguiRev = "${rayguiRevision}"`);
    const defaultUpdated = readFileSync(
      path.join(__dirname, "default.nix")
    ).includes(`version = "${hraylibVersion}"`);

    if (flakeUpdatedRaylib && flakeUpdatedRaygui && defaultUpdated) {
      log(logLevel.INFO, "The flake is already up to date");
      process.exitCode = 0;
      return;
    }

    log(logLevel.VERB, "Deleting flake.lock");

    rmSync(path.join(__dirname, "flake.lock"), {
      force: true
    });

    if (!flakeUpdatedRaylib || !flakeUpdatedRaygui) {
      log(logLevel.INFO, "Prefetching with nix, this might take a while");

      const { hash: raylibHash } = JSON.parse(
        exec(
          `${nixPath} flake prefetch github:raysan5/raylib/${raylibRevision} --extra-experimental-features nix-command --extra-experimental-features flakes --json`
        )
      );
      const { hash: rayguiHash } = JSON.parse(
        exec(
          `${nixPath} flake prefetch github:raysan5/raygui/${rayguiRevision} --extra-experimental-features nix-command --extra-experimental-features flakes --json`
        )
      );

      log(logLevel.INFO, "Prefetched successfully");

      log(logLevel.VERB, "Preparing new flake.nix");

      const flakeTemplate = readFileSync(
        path.join(__dirname, "flake.nix")
      )
        .toString()
        .replace(/raylibRev = "[^"]*"/, `raylibRev = "${raylibRevision}"`)
        .replace(/raylibHash = "[^"]*"/, `raylibHash = "${raylibHash}"`)
        .replace(/rayguiRev = "[^"]*"/, `rayguiRev = "${rayguiRevision}"`)
        .replace(/rayguiHash = "[^"]*"/, `rayguiHash = "${rayguiHash}"`)
      
      log(logLevel.VERB, "Replacing flake.nix");

      rmSync(path.join(__dirname, "flake.nix"));
      writeFileSync(path.join(__dirname, "flake.nix"), flakeTemplate);

      log(logLevel.INFO, "Successfully updated flake.nix");
    }

    if (!defaultUpdated) {
      log(logLevel.VERB, "Preparing new default.nix");

      const defaultTemplate = readFileSync(
        path.join(__dirname, "default.nix")
      )
        .toString()
        .replace(/version = "[^"]*"/, `version = "${hraylibVersion}"`);

      log(logLevel.VERB, "Replacing default.nix");

      rmSync(path.join(__dirname, "default.nix"));
      writeFileSync(path.join(__dirname, "default.nix"), defaultTemplate);

      log(logLevel.INFO, "Successfully updated default.nix");
    }

    log(logLevel.INFO, "Creating flake.lock");

    exec(
      `${nixPath} flake lock --extra-experimental-features nix-command --extra-experimental-features flakes`
    );

    if (existsSync(path.join(__dirname, "flake.lock"))) {
      log(logLevel.INFO, "flake.lock created successfully");
      log(logLevel.INFO, "All done!");
      process.exitCode = 0;
      return;
    } else {
      log(logLevel.ERR, "flake.lock was not created");
      process.exitCode = 1;
    }
  })();
} else if (
  process.argv.includes("-d") ||
  process.argv.includes("--raylib-diff")
) {
  diffWithRemote("raylib");
} else if (
  process.argv.includes("-g") ||
  process.argv.includes("--raygui-diff")
) {
  diffWithRemote("raygui");
} else if (process.argv.includes("-p") || process.argv.includes("--package")) {
  const cabalPath = findCommandPath("cabal", "--cabal-path", "-c");

  log(logLevel.INFO, "Using cabal path: " + cabalPath);
  log(logLevel.VERB, "Cleaning and packaging");

  exec(`${cabalPath} clean`);
  exec(`${cabalPath} sdist`);

  log(logLevel.INFO, "Successfully packaged the project");
  log(logLevel.INFO, "Extracting dist files");
  log(logLevel.VERB, "h-raylib version " + hraylibVersion);

  exec(
    `cd ${path.join(
      __dirname,
      "dist-newstyle",
      "sdist"
    )} && tar -xzf h-raylib-${hraylibVersion}.tar.gz`
  );

  log(logLevel.VERB, "Dist files extracted");
  log(logLevel.VERB, "Copying files");

  const extractPath = path.join(
    __dirname,
    "dist-newstyle",
    "sdist",
    `h-raylib-${hraylibVersion}`
  );

  copyFileSync(
    path.join(__dirname, "cabal.project"),
    path.join(extractPath, "cabal.project")
  );
  copyFileSync(
    path.join(__dirname, "run-all-examples.sh"),
    path.join(extractPath, "run-all-examples.sh")
  );

  log(logLevel.INFO, `Building and running project in ${extractPath}`);

  spawnSync(
    `cd ${extractPath} && sh ${path.join(extractPath, "run-all-examples.sh")}`,
    {
      shell: true,
      stdio: "inherit",
    }
  );
} else {
  const helpText = `
This script contains useful tools for h-raylib development

  Log options

    -v                Verbose logging
    --verbose

    -q                Quiet logging (only errors)
    --quiet

    -s                Silent logging (no logs)
    --silent

  Run types

    -u                Update nix-related files with the latest revision of raylib and latest h-raylib version
    --nix-update

    -d                Diff the local raylib source with the latest remote code
    --raylib-diff

    -g                Diff the local raygui source with the latest remote code
    --raygui-diff

    -p                Package the cabal project and run the examples
    --package

  Additional options

    -n <NIX-PATH>          Manually provide the path to nix; if not provided, the PATH environment variable will be searched
    --nix-path=<NIX-PATH>

    --git-path=<GIT-PATH>  Manually provide the path to git; if not provided, the PATH environment variable will be searched

    -c <CABAL-PATH>        Manually provide the path to cabal; if not provided, the PATH environment variable will be searched
    --cabal-path=<CABAL-PATH>
`;
  console.log(helpText);
}
