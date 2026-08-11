import json
import os

config_path = os.path.expanduser("~/.gemini/config/mcp_config.json")

with open(config_path, "r") as f:
    config = json.load(f)

args = config["mcpServers"]["github-mcp-server"]["args"]

# Check if it's already there
if "--network=host" not in args:
    # Insert it right after --rm (which is at index 2, so insert at 3)
    if "--rm" in args:
        idx = args.index("--rm") + 1
        args.insert(idx, "--network=host")
    else:
        args.insert(1, "--network=host")

with open(config_path, "w") as f:
    json.dump(config, f, indent=2)

print("Successfully updated MCP config with --network=host")
