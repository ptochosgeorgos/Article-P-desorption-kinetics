import json
import os

config_path = os.path.expanduser("~/.gemini/config/mcp_config.json")

with open(config_path, "r") as f:
    config = json.load(f)

# Replace the docker config with npx config
config["mcpServers"]["github-mcp-server"]["command"] = "npx"
config["mcpServers"]["github-mcp-server"]["args"] = ["-y", "@modelcontextprotocol/server-github"]

with open(config_path, "w") as f:
    json.dump(config, f, indent=2)

print("Switched GitHub MCP to npx.")
