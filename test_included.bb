Print "ZARMA"

Select Lower(StrTemp)
	Case "1",""
		CreateConsoleMsg("LIST OF COMMANDS - PAGE 1/3")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("- asd")
		CreateConsoleMsg("- status")
		CreateConsoleMsg("- camerapick")
		CreateConsoleMsg("- ending")
		CreateConsoleMsg("- noclipspeed")
		CreateConsoleMsg("- noclip")
		CreateConsoleMsg("- injure [value]")
		CreateConsoleMsg("- infect [value]")
		CreateConsoleMsg("- heal")
		CreateConsoleMsg("- teleport [room name]")
		CreateConsoleMsg("- spawnitem [item name]")
		CreateConsoleMsg("- wireframe")
		CreateConsoleMsg("- 173speed")
		CreateConsoleMsg("- 106speed")
		CreateConsoleMsg("- 173state")
		CreateConsoleMsg("- 106state")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Use "+Chr(34)+"help 2/3"+Chr(34)+" to find more commands.")
		CreateConsoleMsg("Use "+Chr(34)+"help [command name]"+Chr(34)+" to get more information about a command.")
		CreateConsoleMsg("******************************")
	Case "2"
		CreateConsoleMsg("LIST OF COMMANDS - PAGE 2/3")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("- spawn [npc type] [state]")
		CreateConsoleMsg("- reset096")
		CreateConsoleMsg("- disable173")
		CreateConsoleMsg("- enable173")
		CreateConsoleMsg("- disable106")
		CreateConsoleMsg("- enable106")
		CreateConsoleMsg("- halloween")
		CreateConsoleMsg("- sanic")
		CreateConsoleMsg("- scp-420-j")
		CreateConsoleMsg("- godmode")
		CreateConsoleMsg("- revive")
		CreateConsoleMsg("- noclip")
		CreateConsoleMsg("- showfps")
		CreateConsoleMsg("- 096state")
		CreateConsoleMsg("- debughud")
		CreateConsoleMsg("- camerafog [near] [far]")
		CreateConsoleMsg("- gamma [value]")
		CreateConsoleMsg("- infinitestamina")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Use "+Chr(34)+"help [command name]"+Chr(34)+" to get more information about a command.")
		CreateConsoleMsg("******************************")
	Case "3"
		CreateConsoleMsg("- playmusic [clip + .wav/.ogg]")
		CreateConsoleMsg("- notarget")
		CreateConsoleMsg("- unlockexits")
	Case "asd"
		CreateConsoleMsg("HELP - asd")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Actives godmode, noclip, wireframe and")
		CreateConsoleMsg("sets fog distance to 20 near, 30 far")
		CreateConsoleMsg("******************************")
	Case "camerafog"
		CreateConsoleMsg("HELP - camerafog")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Sets the draw distance of the fog.")
		CreateConsoleMsg("The fog begins generating at 'CameraFogNear' units")
		CreateConsoleMsg("away from the camera and becomes completely opaque")
		CreateConsoleMsg("at 'CameraFogFar' units away from the camera.")
		CreateConsoleMsg("Example: camerafog 20 40")
		CreateConsoleMsg("******************************")
	Case "gamma"
		CreateConsoleMsg("HELP - gamma")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Sets the gamma correction.")
		CreateConsoleMsg("Should be set to a value between 0.0 and 2.0.")
		CreateConsoleMsg("Default is 1.0.")
		CreateConsoleMsg("******************************")
	Case "noclip","fly"
		CreateConsoleMsg("HELP - noclip")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Toggles noclip, unless a valid parameter")
		CreateConsoleMsg("is specified (on/off).")
		CreateConsoleMsg("Allows the camera to move in any direction while")
		CreateConsoleMsg("bypassing collision.")
		CreateConsoleMsg("******************************")
	Case "godmode","god"
		CreateConsoleMsg("HELP - godmode")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Toggles godmode, unless a valid parameter")
		CreateConsoleMsg("is specified (on/off).")
		CreateConsoleMsg("Prevents player death under normal circumstances.")
		CreateConsoleMsg("******************************")
	Case "wireframe"
		CreateConsoleMsg("HELP - wireframe")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Toggles wireframe, unless a valid parameter")
		CreateConsoleMsg("is specified (on/off).")
		CreateConsoleMsg("Allows only the edges of geometry to be rendered,")
		CreateConsoleMsg("making everything else transparent.")
		CreateConsoleMsg("******************************")
	Case "spawnitem"
		CreateConsoleMsg("HELP - spawnitem")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Spawns an item at the player's location.")
		CreateConsoleMsg("Any name that can appear in your inventory")
		CreateConsoleMsg("is a valid parameter.")
		CreateConsoleMsg("Example: spawnitem Key Card Omni")
		CreateConsoleMsg("******************************")
	Case "spawn"
		CreateConsoleMsg("HELP - spawn")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Spawns an NPC at the player's location.")
		CreateConsoleMsg("Valid parameters are:")
		CreateConsoleMsg("008zombie / 049 / 049-2 / 066 / 096 / 106 / 173")
		CreateConsoleMsg("/ 178-1 / 372 / 513-1 / 966 / 1499-1 / class-d")
		CreateConsoleMsg("/ guard / mtf / apache / tentacle")
		CreateConsoleMsg("******************************")
	Case "revive","undead","resurrect"
		CreateConsoleMsg("HELP - revive")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Resets the player's death timer after the dying")
		CreateConsoleMsg("animation triggers.")
		CreateConsoleMsg("Does not affect injury, blood loss")
		CreateConsoleMsg("or 008 infection values.")
		CreateConsoleMsg("******************************")
	Case "teleport"
		CreateConsoleMsg("HELP - teleport")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Teleports the player to the first instance")
		CreateConsoleMsg("of the specified room. Any room that appears")
		CreateConsoleMsg("in rooms.ini is a valid parameter.")
		CreateConsoleMsg("******************************")
	Case "stopsound", "stfu"
		CreateConsoleMsg("HELP - stopsound")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Stops all currently playing sounds.")
		CreateConsoleMsg("******************************")
	Case "camerapick"
		CreateConsoleMsg("HELP - camerapick")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Prints the texture name and coordinates of")
		CreateConsoleMsg("the model the camera is pointing at.")
		CreateConsoleMsg("******************************")
	Case "status"
		CreateConsoleMsg("HELP - status")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Prints player, camera, and room information.")
		CreateConsoleMsg("******************************")
	Case "weed","scp-420-j","420"
		CreateConsoleMsg("HELP - 420")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Generates dank memes.")
		CreateConsoleMsg("******************************")
	Case "playmusic"
		CreateConsoleMsg("HELP - playmusic")
		CreateConsoleMsg("******************************")
		CreateConsoleMsg("Will play tracks in .ogg/.wav format")
		CreateConsoleMsg("from "+Chr(34)+"SFX\Music\Custom\"+Chr(34)+".")
		CreateConsoleMsg("******************************")

	Default
		CreateConsoleMsg("There is no help available for that command.",255,150,0)
End Select
