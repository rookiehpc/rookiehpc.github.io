var mpiSourceCodeC = "";
var mpiSourceCodeF90 = "";
var mpiSourceCodeF08 = "";
var CellCount = 16;
let normalCellBorderColour = "rgb(180, 180, 180)";
let overlapedCellColour = "rgb(150, 150, 150)"; //"rgb(166, 65, 27)";
let occupiedCellColour = "rgb(200, 200, 200)"; //"rgb(251, 101, 44)";
let freeCellColour = "white";

function changeCellCount(newCellCount)
{
	newCellCount = parseInt(newCellCount);
	if(newCellCount > CellCount)
	{
		var Visualisation = document.getElementById('Visualisation');
		for(var i = CellCount; i < newCellCount; i++)
		{
			var newChild = document.createElement("div");
			newChild.id = i;
			newChild.classList.add("Cell");
			Visualisation.appendChild(newChild);
		}
	}
	else if(newCellCount < CellCount)
	{
		for(var i = newCellCount; i < CellCount; i++)
		{
			var element = document.getElementById(i);
			element.parentNode.removeChild(element);
		}
	}
	CellCount = newCellCount;

	// Reset borders
	for(var i = 0; i < CellCount; i++)
	{
		document.getElementById(i).style.border = "2px solid " + normalCellBorderColour;
		document.getElementById(i).style.backgroundColor = freeCellColour;
	}
}

function pickForm()
{
	var allForms = document.getElementsByClassName('RoutineForm');
	for(var i = 0; i < allForms.length; i++)
	{
		allForms[i].style.display = 'none';
	}

	var allLegends = document.getElementsByClassName('RoutineLegend');
	for(var i = 0; i < allLegends.length; i++)
	{
		allLegends[i].style.display = 'none';
	}

	var routineSelected = document.getElementById('routine');
	var routineName = routineSelected.options[routineSelected.selectedIndex].value;
	//var text = routineSelected.options[routineSelected.selectedIndex].text;
	var formSelected = document.getElementById(routineName + "Form");
	formSelected.style.display = 'inherit';

	var legendSelected = document.getElementById(routineName + "Legend");
	legendSelected.style.display = 'inherit';	

	// Trigger display update
	update_display();
}

function updateOldDatatypeName(oldDatatypeName)
{
	var all_oldDatatypeNames = document.getElementsByClassName('oldDatatypeName');
	for(var i = 0; i < all_oldDatatypeNames.length; i++)
	{
		all_oldDatatypeNames[i].innerHTML = oldDatatypeName;
	}
}

function update_display()
{
	var routineSelected = document.getElementById('routine');
	var routineName = routineSelected.options[routineSelected.selectedIndex].value;
	var formSelected = document.getElementById(routineName + "Form");
	formSelected.style.display = 'inherit';

	if(routineName == 'MPI_Type_contiguous')
	{
		var replication_count = document.getElementById(routineName + "Count").value;
		if(replication_count != "")
		{
			var newDatatypeName = document.getElementById(routineName + "NewDatatype").value;
			var oldDatatypeName = document.getElementById(routineName + "OldDatatype").value;
			updateOldDatatypeName(oldDatatypeName);

			changeCellCount(replication_count);
			var replication_count_i = 0;
			while(replication_count_i < replication_count)
			{
				document.getElementById(replication_count_i).style.backgroundColor = occupiedCellColour;
				replication_count_i++;
			}

			// Update the MPI call
			mpiSourceCodeC = "MPI_Datatype " + newDatatypeName + ";\n" + routineName + "(" + replication_count + ", " + oldDatatypeName + ", &" + newDatatypeName + ");\nMPI_Type_commit(&" + newDatatypeName + ");";
			mpiSourceCodeF90 = "INTEGER :: " + newDatatypeName + ", ierror\nCALL " + routineName + "(" + replication_count + ", " + oldDatatypeName + ", " + newDatatypeName + ", ierror)\nCALL MPI_Type_commit(" + newDatatypeName + ", ierror)";
			mpiSourceCodeF08 = "TYPE(MPI_Datatype) :: " + newDatatypeName + "\nCALL " + routineName + "(" + replication_count + ", " + oldDatatypeName + ", " + newDatatypeName + ")\nCALL MPI_Type_commit(" + newDatatypeName + ")";
		}
	}
	else if(routineName == 'MPI_Type_vector')
	{
		// Get all parameter values
		var blockCount = document.getElementById(routineName + "BlockCount").value;
		var blockLength = document.getElementById(routineName + "BlockLength").value;
		var stride = document.getElementById(routineName + "Stride").value;

		// Update the Visualisation
		if(blockCount != "" && blockLength != "" && stride != "")
		{
			var newDatatypeName = document.getElementById(routineName + "NewDatatype").value;
			var oldDatatypeName = document.getElementById(routineName + "OldDatatype").value;
			updateOldDatatypeName(oldDatatypeName);

			changeCellCount(parseInt(stride) * parseInt(blockCount-1) + parseInt(blockLength));

			var blockCountI = 0;
			var blockLengthI = 0;
			var lastBlockOffset = 0;
			while(blockCountI < blockCount)
			{
				blockLengthI = 0;
				while(blockLengthI < blockLength)
				{
					if(document.getElementById(parseInt(lastBlockOffset) + parseInt(blockLengthI)).style.backgroundColor == freeCellColour)
					{
						document.getElementById(parseInt(lastBlockOffset) + parseInt(blockLengthI)).style.backgroundColor = occupiedCellColour;
					}
					else
					{
						document.getElementById(parseInt(lastBlockOffset) + parseInt(blockLengthI)).style.backgroundColor = overlapedCellColour;
					}
					blockLengthI++;
				}
				blockCountI++;
				lastBlockOffset += parseInt(stride);
			}

			// Update the MPI call
			mpiSourceCodeC = "MPI_Datatype " + newDatatypeName + ";\n" + routineName + "(" + blockCount + ", " + blockLength + ", " + stride + ", " + oldDatatypeName + ", &" + newDatatypeName + ");\nMPI_Type_commit(&" + newDatatypeName + ");";
			mpiSourceCodeF90 = "INTEGER :: " + newDatatypeName + ", ierror\nCALL " + routineName + "(" + blockCount + ", " + blockLength + ", " + stride + ", " + oldDatatypeName + ", " + newDatatypeName + ", ierror)\nCALL MPI_Type_commit(" + newDatatypeName + ", ierror)";
			mpiSourceCodeF08 = "TYPE(MPI_Datatype) :: " + newDatatypeName + "\nCALL " + routineName + "(" + blockCount + ", " + blockLength + ", " + stride + ", " + oldDatatypeName + ", " + newDatatypeName + ")\nCALL MPI_Type_commit(" + newDatatypeName + ")";
		}
	}
	else if(routineName == 'MPI_Type_create_hvector')
	{
		// Get all parameter values
		var oldDatatypeLength = parseInt(document.getElementById(routineName + "OldDatatypeLength").value);
		var blockCount = parseInt(document.getElementById(routineName + "BlockCount").value);
		var blockLength = parseInt(document.getElementById(routineName + "BlockLength").value);
		var stride = parseInt(document.getElementById(routineName + "Stride").value);
		var newDatatypeName = document.getElementById(routineName + "NewDatatype").value;
		var oldDatatypeName = document.getElementById(routineName + "OldDatatype").value;
		updateOldDatatypeName(oldDatatypeName);

		// Update the legend
		var allOldDatatypeNames = document.getElementById(routineName + "Legend").getElementsByClassName("oldDatatypeLength");
		for(var i = 0; i < allOldDatatypeNames.length; i++)
		{
			allOldDatatypeNames[i].innerHTML = oldDatatypeLength;
		}

		// Set the number of bytes in the old datatype
		var allOldDatatypeBytes = document.getElementsByClassName(routineName + "OldDatatypeByte");
		if(allOldDatatypeBytes.length > oldDatatypeLength)
		{
			for(var i = oldDatatypeLength; i < allOldDatatypeBytes.length; i++)
			{
				allOldDatatypeBytes[i].parentNode.removeChild(allOldDatatypeBytes[i]);
			}
		}
		else
		{
			for(var i = allOldDatatypeBytes.length; i < oldDatatypeLength; i++)
			{
				var newOldDatatypeByte = allOldDatatypeBytes[allOldDatatypeBytes.length-1].cloneNode(true);
				allOldDatatypeBytes[allOldDatatypeBytes.length-1].parentNode.insertBefore(newOldDatatypeByte, allOldDatatypeBytes[allOldDatatypeBytes.length-1].nextSibling);
			}
		}

		allOldDatatypeBytes = document.getElementsByClassName(routineName + "OldDatatypeByte");
		for(var i = 0; i < allOldDatatypeBytes.length; i++)
		{
			if(i == allOldDatatypeBytes.length - 1)
			{
				allOldDatatypeBytes[allOldDatatypeBytes.length - 1].style.marginRight = "10px";
			}
			else
			{
				allOldDatatypeBytes[i].style.marginRight = "2px";
			}
		}

		// Update the Visualisation
		changeCellCount(stride * (blockCount-1) + blockLength * oldDatatypeLength);

		var blockCountI = 0;
		var lastBlockOffset = 0;
		while(blockCountI < blockCount)
		{
			for(var blockLengthI = 0; blockLengthI < blockLength; blockLengthI++)
			{
				for(var oldDatatypeLengthI = 0; oldDatatypeLengthI < oldDatatypeLength; oldDatatypeLengthI++)
				{
					var currentIndex = parseInt(lastBlockOffset) + parseInt(blockLengthI) * oldDatatypeLength + parseInt(oldDatatypeLengthI);
					if(document.getElementById(currentIndex).style.backgroundColor == freeCellColour)
					{
						document.getElementById(currentIndex).style.backgroundColor = occupiedCellColour;
					}
					else
					{
						document.getElementById(currentIndex).style.backgroundColor = overlapedCellColour;
					}
				}
			}
			blockCountI++;
			lastBlockOffset += stride;
		}

		// Update the MPI call
		mpiSourceCodeC = "MPI_Datatype " + newDatatypeName + ";\n" + routineName + "(" + blockCount + ", " + blockLength + ", " + stride + ", " + oldDatatypeName + ", &" + newDatatypeName + ");\nMPI_Type_commit(&" + newDatatypeName + ");";
		mpiSourceCodeF90 = "INTEGER :: " + newDatatypeName + ", ierror\nCALL " + routineName + "(" + blockCount + ", " + blockLength + ", " + stride + ", " + oldDatatypeName + ", " + newDatatypeName + ", ierror)\nCALL MPI_Type_commit(" + newDatatypeName + ", ierror)";
		mpiSourceCodeF08 = "TYPE(MPI_Datatype) :: " + newDatatypeName + "\nCALL " + routineName + "(" + blockCount + ", " + blockLength + ", " + stride + ", " + oldDatatypeName + ", " + newDatatypeName + ")\nCALL MPI_Type_commit(" + newDatatypeName + ")";
	}
	else if(routineName == "MPI_Type_indexed")
	{
		var newDatatypeName = document.getElementById(routineName + "NewDatatype").value;
		var oldDatatypeName = document.getElementById(routineName + "OldDatatype").value;
		updateOldDatatypeName(oldDatatypeName);

		var blockCount = document.getElementById(routineName + "BlockCount").value;

		// Delete extraneous block lengths and block offsets in case the block count has been decreased
		var blockCountI = blockCount;
		var blockLengthToDelete = document.getElementById(routineName + "BlockLength" + blockCountI);
		var blockOffsetToDelete = document.getElementById(routineName + "BlockOffset" + blockCountI);
		while(blockLengthToDelete)
		{
			// Delete the block and take next
			blockLengthToDelete.parentNode.removeChild(blockLengthToDelete);
			blockOffsetToDelete.parentNode.removeChild(blockOffsetToDelete);
			blockCountI++;
			blockLengthToDelete = document.getElementById(routineName + "BlockLength" + blockCountI);
			blockOffsetToDelete = document.getElementById(routineName + "BlockOffset" + blockCountI);
		}

		// Add missing blocks in case the block count has been increased
		var lastBlockLength = document.getElementById(routineName + "BlockLength0");
		var lastBlockOffset = document.getElementById(routineName + "BlockOffset0");
		for(var i = 0; i < blockCount; i++)
		{
			var blockLengthToAdd = document.getElementById(routineName + "BlockLength" + i);
			var blockOffsetToAdd = document.getElementById(routineName + "BlockOffset" + i);
			if(!blockLengthToAdd)
			{
				blockLengthToAdd = document.createElement('input');
				blockLengthToAdd.id = routineName + "BlockLength" + i;
				blockLengthToAdd.type = "number";
				blockLengthToAdd.value = 3;
				blockLengthToAdd.min = 1;
				blockLengthToAdd.onchange = update_display;
				lastBlockLength.parentNode.insertBefore(blockLengthToAdd, lastBlockLength.nextSibling);

				// Same for the block offset
				blockOffsetToAdd = document.createElement('input');
				blockOffsetToAdd.id = routineName + "BlockOffset" + i;
				blockOffsetToAdd.type = "number";
				blockOffsetToAdd.value = parseInt(lastBlockOffset.value) + parseInt(blockLengthToAdd.value) + 1;
				blockOffsetToAdd.min = 0;
				blockOffsetToAdd.onchange = update_display;
				lastBlockOffset.parentNode.insertBefore(blockOffsetToAdd, lastBlockOffset.nextSibling);
			}
			lastBlockLength = blockLengthToAdd;
			lastBlockOffset = blockOffsetToAdd;
		}

		// Get number of Cells needed
		var maxNumberOfCellsNeeded = 0;
		for(var i = 0; i < blockCount; i++)
		{
			var blockLength = parseInt(document.getElementById(routineName + "BlockLength" + i).value);
			var blockOffset = parseInt(document.getElementById(routineName + "BlockOffset" + i).value);
			if(maxNumberOfCellsNeeded < blockLength + blockOffset)
			{
				maxNumberOfCellsNeeded = blockLength + blockOffset;
			}
		}

		changeCellCount(maxNumberOfCellsNeeded);
		for(var i = 0; i < CellCount; i++)
		{
			document.getElementById(i).style.backgroundColor = freeCellColour;
		}
		for(var i = 0; i < blockCount; i++)
		{
			var blockLength = parseInt(document.getElementById(routineName + "BlockLength" + i).value);
			var blockOffset = parseInt(document.getElementById(routineName + "BlockOffset" + i).value);
			for(var j = 0; j < blockLength; j++)
			{
				if(document.getElementById(blockOffset + j).style.backgroundColor == freeCellColour )
				{
					document.getElementById(blockOffset + j).style.backgroundColor = occupiedCellColour;
				}
				else
				{
					document.getElementById(blockOffset + j).style.backgroundColor = overlapedCellColour;
				}
			}
		}

		// Update the MPI call
		mpiSourceCodeC = "int lengths[" + blockCount + "] = {" + document.getElementById(routineName + "BlockLength" + "0").value;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeC += ", " + document.getElementById(routineName + "BlockLength" + i).value;
		}
		mpiSourceCodeC += "};\nint displacements[" + blockCount + "] = {" + document.getElementById(routineName + "BlockOffset" + "0").value;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeC += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeC += "};\n" + "MPI_Datatype " + newDatatypeName + ";\n" + routineName + "(" + blockCount + ", lengths, displacements, " + oldDatatypeName + ", &" + newDatatypeName + ");\nMPI_Type_commit(&" + newDatatypeName + ");";

		// FORTRAN-legacy version
		mpiSourceCodeF90 = "INTEGER :: lengths(3) = (/" + document.getElementById(routineName + "BlockLength0").value;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF90 += ", " + document.getElementById(routineName + "BlockLength" + i).value;
		}
		mpiSourceCodeF90 += "/), displacements(3) = (/" + document.getElementById(routineName + "BlockOffset0").value;;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF90 += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeF90 += "/), " + newDatatypeName + ", ierror\nCALL " + routineName + "(" + blockCount + ", lengths, displacements, " + oldDatatypeName + ", " + newDatatypeName + ", ierror)\nCALL MPI_Type_commit(" + newDatatypeName + ", ierror)";

		// FORTRAN-2008 version
		mpiSourceCodeF08 = "INTEGER :: lengths(3) = [" + document.getElementById(routineName + "BlockLength0").value;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF08 += ", " + document.getElementById(routineName + "BlockLength" + i).value;
		}
		mpiSourceCodeF08 += "], displacements(3) = [" + document.getElementById(routineName + "BlockOffset0").value;;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF08 += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeF08 += "]\nTYPE(MPI_Datatype) :: " + newDatatypeName + "\nCALL " + routineName + "(" + blockCount + ", lengths, displacements, " + oldDatatypeName + ", " + newDatatypeName + ")\nCALL MPI_Type_commit(" + newDatatypeName + ")";
	}
	else if(routineName == "MPI_Type_create_hindexed")
	{
		var newDatatypeName = document.getElementById(routineName + "NewDatatype").value;
		var oldDatatypeName = document.getElementById(routineName + "OldDatatype").value;
		updateOldDatatypeName(oldDatatypeName);

		var blockCount = parseInt(document.getElementById(routineName + "BlockCount").value);
		var oldDatatypeLength = parseInt(document.getElementById(routineName + "OldDatatypeLength").value);
		if(!blockCount || !oldDatatypeLength)
			return;

		// Update the legend
		var allOldDatatypeNames = document.getElementById(routineName + "Legend").getElementsByClassName("oldDatatypeLength");
		for(var i = 0; i < allOldDatatypeNames.length; i++)
		{
			allOldDatatypeNames[i].innerHTML = oldDatatypeLength;
		}

		// Delete extraneous block lengths and block offsets in case the block count has been decreased
		var blockCountI = blockCount;
		var blockLengthToDelete = document.getElementById(routineName + "BlockLength" + blockCountI);
		var blockOffsetToDelete = document.getElementById(routineName + "BlockOffset" + blockCountI);
		while(blockLengthToDelete)
		{
			// Delete the block and take next
			blockLengthToDelete.parentNode.removeChild(blockLengthToDelete);
			blockOffsetToDelete.parentNode.removeChild(blockOffsetToDelete);
			blockCountI++;
			blockLengthToDelete = document.getElementById(routineName + "BlockLength" + blockCountI);
			blockOffsetToDelete = document.getElementById(routineName + "BlockOffset" + blockCountI);
		}

		// Add missing blocks in case the block count has been increased
		var lastBlockLength = document.getElementById(routineName + "BlockLength0");
		var lastBlockOffset = document.getElementById(routineName + "BlockOffset0");
		for(var i = 0; i < blockCount; i++)
		{
			var blockLengthToAdd = document.getElementById(routineName + "BlockLength" + i);
			var blockOffsetToAdd = document.getElementById(routineName + "BlockOffset" + i);
			if(!blockLengthToAdd)
			{
				blockLengthToAdd = document.createElement('input');
				blockLengthToAdd.id = routineName + "BlockLength" + i;
				blockLengthToAdd.type = "number";
				blockLengthToAdd.value = 3;
				blockLengthToAdd.min = 1;
				blockLengthToAdd.onchange = update_display;
				lastBlockLength.parentNode.insertBefore(blockLengthToAdd, lastBlockLength.nextSibling);

				// Same for the block offset
				blockOffsetToAdd = document.createElement('input');
				blockOffsetToAdd.id = routineName + "BlockOffset" + i;
				blockOffsetToAdd.type = "number";
				blockOffsetToAdd.value = parseInt(lastBlockOffset.value) + parseInt(blockLengthToAdd.value * oldDatatypeLength) + 1;
				blockOffsetToAdd.min = 0;
				blockOffsetToAdd.onchange = update_display;
				lastBlockOffset.parentNode.insertBefore(blockOffsetToAdd, lastBlockOffset.nextSibling);
			}
			lastBlockLength = blockLengthToAdd;
			lastBlockOffset = blockOffsetToAdd;
		}

		// Set the number of bytes in the old datatype
		var allOldDatatypeBytes = document.getElementsByClassName(routineName + "OldDatatypeByte");
		if(allOldDatatypeBytes.length > oldDatatypeLength)
		{
			for(var i = oldDatatypeLength; i < allOldDatatypeBytes.length; i++)
			{
				allOldDatatypeBytes[i].parentNode.removeChild(allOldDatatypeBytes[i]);
			}
		}
		else
		{
			for(var i = allOldDatatypeBytes.length; i < oldDatatypeLength; i++)
			{
				var newOldDatatypeByte = allOldDatatypeBytes[allOldDatatypeBytes.length-1].cloneNode(true);
				allOldDatatypeBytes[allOldDatatypeBytes.length-1].parentNode.insertBefore(newOldDatatypeByte, allOldDatatypeBytes[allOldDatatypeBytes.length-1].nextSibling);
			}
		}

		allOldDatatypeBytes = document.getElementsByClassName(routineName + "OldDatatypeByte");
		for(var i = 0; i < allOldDatatypeBytes.length; i++)
		{
			if(i == allOldDatatypeBytes.length - 1)
			{
				allOldDatatypeBytes[allOldDatatypeBytes.length - 1].style.marginRight = "10px";
			}
			else
			{
				allOldDatatypeBytes[i].style.marginRight = "2px";
			}
		}

		// Get number of Cells needed
		var maxNumberOfCellsNeeded = 0;
		for(var i = 0; i < blockCount; i++)
		{
			var blockLength = parseInt(document.getElementById(routineName + "BlockLength" + i).value);
			var blockOffset = parseInt(document.getElementById(routineName + "BlockOffset" + i).value);
			if(maxNumberOfCellsNeeded < blockLength * oldDatatypeLength + blockOffset)
			{
				maxNumberOfCellsNeeded = blockLength * oldDatatypeLength + blockOffset;
			}
		}

		changeCellCount(maxNumberOfCellsNeeded);
		for(var i = 0; i < CellCount; i++)
		{
			document.getElementById(i).style.backgroundColor = freeCellColour;
			document.getElementById(i).style.borderRight = "2px solid " + normalCellBorderColour;
			document.getElementById(i).style.borderLeft = "2px solid " + normalCellBorderColour;
		}
		for(var i = 0; i < blockCount; i++)
		{
			var blockLength = parseInt(document.getElementById(routineName + "BlockLength" + i).value);
			var blockOffset = parseInt(document.getElementById(routineName + "BlockOffset" + i).value);
			for(var j = 0; j < blockLength; j++)
			{
				for(var k = 0; k < oldDatatypeLength; k++)
				{
					if(document.getElementById(blockOffset + j * oldDatatypeLength + k).style.backgroundColor == freeCellColour )
					{
						document.getElementById(blockOffset + j * oldDatatypeLength + k).style.backgroundColor = occupiedCellColour;
					}
					else
					{
						document.getElementById(blockOffset + j * oldDatatypeLength + k).style.backgroundColor = overlapedCellColour;
					}
				}
			}
		}

		// Update the MPI call
		mpiSourceCodeC = "int lengths[" + blockCount + "] = {" + document.getElementById(routineName + "BlockLength" + "0").value;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeC += ", " + document.getElementById(routineName + "BlockLength" + i).value;
		}
		mpiSourceCodeC += "};\nint displacements[" + blockCount + "] = {" + document.getElementById(routineName + "BlockOffset" + "0").value;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeC += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeC += "};\n" + "MPI_Datatype " + newDatatypeName + ";\n" + routineName + "(" + blockCount + ", lengths, displacements, " + oldDatatypeName + ", &" + newDatatypeName + ");\nMPI_Type_commit(&" + newDatatypeName + ");";

		// FORTRAN-legacy version
		mpiSourceCodeF90 = "INTEGER :: lengths(3) = (/" + document.getElementById(routineName + "BlockLength0").value;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF90 += ", " + document.getElementById(routineName + "BlockLength" + i).value;
		}
		mpiSourceCodeF90 += "/), displacements(3) = (/" + document.getElementById(routineName + "BlockOffset0").value;;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF90 += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeF90 += "/), " + newDatatypeName + ", ierror\nCALL " + routineName + "(" + blockCount + ", lengths, displacements, " + oldDatatypeName + ", " + newDatatypeName + ", ierror)\nCALL MPI_Type_commit(" + newDatatypeName + ", ierror)";

		// FORTRAN-2008 version
		mpiSourceCodeF08 = "INTEGER :: lengths(3) = [" + document.getElementById(routineName + "BlockLength0").value;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF08 += ", " + document.getElementById(routineName + "BlockLength" + i).value;
		}
		mpiSourceCodeF08 += "], displacements(3) = [" + document.getElementById(routineName + "BlockOffset0").value;;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF08 += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeF08 += "]\nTYPE(MPI_Datatype) :: " + newDatatypeName + "\nCALL " + routineName + "(" + blockCount + ", lengths, displacements, " + oldDatatypeName + ", " + newDatatypeName + ")\nCALL MPI_Type_commit(" + newDatatypeName + ")";
	}
	else if(routineName == "MPI_Type_create_indexed_block")
	{
		var newDatatypeName = document.getElementById(routineName + "NewDatatype").value;
		var oldDatatypeName = document.getElementById(routineName + "OldDatatype").value;
		updateOldDatatypeName(oldDatatypeName);

		var blockCount = document.getElementById(routineName + "BlockCount").value;
		var blockLength = parseInt(document.getElementById(routineName + "BlockLength").value);

		// Delete extraneous block lengths and block offsets in case the block count has been decreased
		var blockCountI = blockCount;
		var blockOffsetToDelete = document.getElementById(routineName + "BlockOffset" + blockCountI);
		while(blockOffsetToDelete)
		{
			// Delete the block and take next
			blockOffsetToDelete.parentNode.removeChild(blockOffsetToDelete);
			blockCountI++;
			blockOffsetToDelete = document.getElementById(routineName + "BlockOffset" + blockCountI);
		}

		// Add missing blocks in case the block count has been increased
		var lastBlockOffset = document.getElementById(routineName + "BlockOffset0");
		for(var i = 1; i < blockCount; i++)
		{
			var blockOffsetToAdd = document.getElementById(routineName + "BlockOffset" + i);
			if(!blockOffsetToAdd)
			{
				blockOffsetToAdd = document.createElement('input');
				blockOffsetToAdd.id = routineName + "BlockOffset" + i;
				blockOffsetToAdd.type = "number";
				blockOffsetToAdd.value = parseInt(lastBlockOffset.value) + blockLength + 1;
				blockOffsetToAdd.min = 0;
				blockOffsetToAdd.onchange = update_display;
				lastBlockOffset.parentNode.insertBefore(blockOffsetToAdd, lastBlockOffset.nextSibling);
			}
			lastBlockOffset = blockOffsetToAdd;
		}

		// Get number of Cells needed
		var maxNumberOfCellsNeeded = 0;
		for(var i = 0; i < blockCount; i++)
		{
			var blockOffset = parseInt(document.getElementById(routineName + "BlockOffset" + i).value);
			if(maxNumberOfCellsNeeded < blockLength + blockOffset)
			{
				maxNumberOfCellsNeeded = blockLength + blockOffset;
			}
		}

		changeCellCount(maxNumberOfCellsNeeded);
		for(var i = 0; i < CellCount; i++)
		{
			document.getElementById(i).style.backgroundColor = freeCellColour;
		}
		for(var i = 0; i < blockCount; i++)
		{
			var blockOffset = parseInt(document.getElementById(routineName + "BlockOffset" + i).value);
			for(var j = 0; j < blockLength; j++)
			{
				if(document.getElementById(blockOffset + j).style.backgroundColor == freeCellColour )
				{
					document.getElementById(blockOffset + j).style.backgroundColor = occupiedCellColour;
				}
				else
				{
					document.getElementById(blockOffset + j).style.backgroundColor = overlapedCellColour;
				}
			}
		}

		// Update the MPI call
		mpiSourceCodeC = "int lengths[" + blockCount + "] = {" + document.getElementById(routineName + "BlockLength").value + "};\nint displacements[" + blockCount + "] = {" + document.getElementById(routineName + "BlockOffset" + "0").value;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeC += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeC += "};\n" + "MPI_Datatype " + newDatatypeName + ";\n" + routineName + "(" + blockCount + ", lengths, displacements, " + oldDatatypeName + ", &" + newDatatypeName + ");\nMPI_Type_commit(&" + newDatatypeName + ");";

		// FORTRAN-legacy version
		mpiSourceCodeF90 = "INTEGER :: lengths(3) = (/" + document.getElementById(routineName + "BlockLength").value + "/), displacements(3) = (/" + document.getElementById(routineName + "BlockOffset0").value;;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF90 += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeF90 += "/), " + newDatatypeName + ", ierror\nCALL " + routineName + "(" + blockCount + ", lengths, displacements, " + oldDatatypeName + ", " + newDatatypeName + ", ierror)\nCALL MPI_Type_commit(" + newDatatypeName + ", ierror)";

		// FORTRAN-2008 version
		mpiSourceCodeF08 = "INTEGER :: lengths(3) = [" + document.getElementById(routineName + "BlockLength").value + "], displacements(3) = [" + document.getElementById(routineName + "BlockOffset0").value;;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF08 += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeF08 += "]\nTYPE(MPI_Datatype) :: " + newDatatypeName + "\nCALL " + routineName + "(" + blockCount + ", lengths, displacements, " + oldDatatypeName + ", " + newDatatypeName + ")\nCALL MPI_Type_commit(" + newDatatypeName + ")";
	}
	else if(routineName == "MPI_Type_create_hindexed_block")
	{
		var newDatatypeName = document.getElementById(routineName + "NewDatatype").value;
		var oldDatatypeName = document.getElementById(routineName + "OldDatatype").value;
		updateOldDatatypeName(oldDatatypeName);

		var blockCount = parseInt(document.getElementById(routineName + "BlockCount").value);
		var oldDatatypeLength = parseInt(document.getElementById(routineName + "OldDatatypeLength").value);
		if(!blockCount || !oldDatatypeLength)
			return;

		// Update the legend
		var allOldDatatypeNames = document.getElementById(routineName + "Legend").getElementsByClassName("oldDatatypeLength");
		for(var i = 0; i < allOldDatatypeNames.length; i++)
		{
			allOldDatatypeNames[i].innerHTML = oldDatatypeLength;
		}

		// Delete extraneous block lengths and block offsets in case the block count has been decreased
		var blockCountI = blockCount;
		var blockLength = parseInt(document.getElementById(routineName + "BlockLength").value);
		var blockOffsetToDelete = document.getElementById(routineName + "BlockOffset" + blockCountI);
		while(blockOffsetToDelete)
		{
			// Delete the block and take next
			blockOffsetToDelete.parentNode.removeChild(blockOffsetToDelete);
			blockCountI++;
			blockOffsetToDelete = document.getElementById(routineName + "BlockOffset" + blockCountI);
		}

		// Add missing blocks in case the block count has been increased
		var lastBlockOffset = document.getElementById(routineName + "BlockOffset0");
		for(var i = 0; i < blockCount; i++)
		{
			var blockOffsetToAdd = document.getElementById(routineName + "BlockOffset" + i);
			if(!blockOffsetToAdd)
			{
				// Same for the block offset
				blockOffsetToAdd = document.createElement('input');
				blockOffsetToAdd.id = routineName + "BlockOffset" + i;
				blockOffsetToAdd.type = "number";
				blockOffsetToAdd.value = parseInt(lastBlockOffset.value) + parseInt(blockLength * oldDatatypeLength) + 1;
				blockOffsetToAdd.min = 0;
				blockOffsetToAdd.onchange = update_display;
				lastBlockOffset.parentNode.insertBefore(blockOffsetToAdd, lastBlockOffset.nextSibling);
			}
			lastBlockOffset = blockOffsetToAdd;
		}

		// Set the number of bytes in the old datatype
		var allOldDatatypeBytes = document.getElementsByClassName(routineName + "OldDatatypeByte");
		if(allOldDatatypeBytes.length > oldDatatypeLength)
		{
			for(var i = oldDatatypeLength; i < allOldDatatypeBytes.length; i++)
			{
				allOldDatatypeBytes[i].parentNode.removeChild(allOldDatatypeBytes[i]);
			}
		}
		else
		{
			for(var i = allOldDatatypeBytes.length; i < oldDatatypeLength; i++)
			{
				var newOldDatatypeByte = allOldDatatypeBytes[allOldDatatypeBytes.length-1].cloneNode(true);
				allOldDatatypeBytes[allOldDatatypeBytes.length-1].parentNode.insertBefore(newOldDatatypeByte, allOldDatatypeBytes[allOldDatatypeBytes.length-1].nextSibling);
			}
		}

		allOldDatatypeBytes = document.getElementsByClassName(routineName + "OldDatatypeByte");
		for(var i = 0; i < allOldDatatypeBytes.length; i++)
		{
			if(i == allOldDatatypeBytes.length - 1)
			{
				allOldDatatypeBytes[allOldDatatypeBytes.length - 1].style.marginRight = "10px";
			}
			else
			{
				allOldDatatypeBytes[i].style.marginRight = "2px";
			}
		}

		// Get number of Cells needed
		var maxNumberOfCellsNeeded = 0;
		for(var i = 0; i < blockCount; i++)
		{
			var blockOffset = parseInt(document.getElementById(routineName + "BlockOffset" + i).value);
			if(maxNumberOfCellsNeeded < blockLength * oldDatatypeLength + blockOffset)
			{
				maxNumberOfCellsNeeded = blockLength * oldDatatypeLength + blockOffset;
			}
		}

		changeCellCount(maxNumberOfCellsNeeded);
		for(var i = 0; i < CellCount; i++)
		{
			document.getElementById(i).style.backgroundColor = freeCellColour;
			document.getElementById(i).style.borderRight = "2px solid " + normalCellBorderColour;
			document.getElementById(i).style.borderLeft = "2px solid " + normalCellBorderColour;
		}
		for(var i = 0; i < blockCount; i++)
		{
			var blockOffset = parseInt(document.getElementById(routineName + "BlockOffset" + i).value);
			for(var j = 0; j < blockLength; j++)
			{
				for(var k = 0; k < oldDatatypeLength; k++)
				{
					if(document.getElementById(blockOffset + j * oldDatatypeLength + k).style.backgroundColor == freeCellColour )
					{
						document.getElementById(blockOffset + j * oldDatatypeLength + k).style.backgroundColor = occupiedCellColour;
					}
					else
					{
						document.getElementById(blockOffset + j * oldDatatypeLength + k).style.backgroundColor = overlapedCellColour;
					}
				}
			}
		}

		// Update the MPI call
		mpiSourceCodeC = "int length = " + document.getElementById(routineName + "BlockLength").value + ";\nint displacements[" + blockCount + "] = {" + document.getElementById(routineName + "BlockOffset" + "0").value;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeC += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeC += "};\n" + "MPI_Datatype " + newDatatypeName + ";\n" + routineName + "(" + blockCount + ", length, displacements, " + oldDatatypeName + ", &" + newDatatypeName + ");\nMPI_Type_commit(&" + newDatatypeName + ");";

		// FORTRAN-legacy version
		mpiSourceCodeF90 = "INTEGER :: length = " + document.getElementById(routineName + "BlockLength").value +", displacements(" + blockCount + ") = (/" + document.getElementById(routineName + "BlockOffset0").value;;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF90 += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeF90 += "/), " + newDatatypeName + ", ierror\nCALL " + routineName + "(" + blockCount + ", length, displacements, " + oldDatatypeName + ", " + newDatatypeName + ", ierror)\nCALL MPI_Type_commit(" + newDatatypeName + ", ierror)";

		// FORTRAN-2008 version
		mpiSourceCodeF08 = "INTEGER :: length = " + document.getElementById(routineName + "BlockLength").value + ", displacements(" + blockCount + ") = [" + document.getElementById(routineName + "BlockOffset0").value;;
		for(var i = 1; i < blockCount; i++)
		{
			mpiSourceCodeF08 += ", " + document.getElementById(routineName + "BlockOffset" + i).value;
		}
		mpiSourceCodeF08 += "]\nTYPE(MPI_Datatype) :: " + newDatatypeName + "\nCALL " + routineName + "(" + blockCount + ", length, displacements, " + oldDatatypeName + ", " + newDatatypeName + ")\nCALL MPI_Type_commit(" + newDatatypeName + ")";
	}

	var cCode = document.getElementById('CSourceCode');
	cCode.firstChild.innerHTML = RK.HighlighCode(mpiSourceCodeC, RK.LANGUAGE_HLJS_CLASS.C);

	var f90Code = document.getElementById('Fortran-90SourceCode');
	f90Code.firstChild.innerHTML = RK.HighlighCode(mpiSourceCodeF90, RK.LANGUAGE_HLJS_CLASS.F90);

	var f08Code = document.getElementById('Fortran-2008SourceCode');
	f08Code.firstChild.innerHTML = RK.HighlighCode(mpiSourceCodeF08, RK.LANGUAGE_HLJS_CLASS.F08);
}

function OnceWindowIsLoaded() {
    // Create the version tabs
    const VersionTabsElement = document.getElementById("VersionTabs");

    const VersionTabContainer = document.createElement("p");
    VersionTabsElement.appendChild(VersionTabContainer);

    let FirstVersionTab = true;
    [RK.LANGUAGES.C, RK.LANGUAGES.F90, RK.LANGUAGES.F08].forEach((Language) => {
        if(FirstVersionTab) {
            FirstVersionTab = false;
        }
        else {
            VersionTabContainer.appendChild(document.createTextNode(" | "));
        }
        VersionTabSpan = document.createElement("span");
        VersionTabSpan.classList.add("VersionTab", "FakeButton", Language);
        VersionTabSpan.textContent = Language;
        VersionTabSpan.addEventListener('click', (Event) => {
            RK.ShowVersion(Event, Language);
        });
        VersionTabContainer.appendChild(VersionTabSpan);
    });

    const SourceCodesElement = document.getElementById("SourceCodes").getElementsByClassName("PageArticleBody")[0];
    RK.CreateCode(" ", SourceCodesElement, RK.LANGUAGE_HLJS_CLASS.C);
    RK.CreateCode(" ", SourceCodesElement, RK.LANGUAGE_HLJS_CLASS.F90);
    RK.CreateCode(" ", SourceCodesElement, RK.LANGUAGE_HLJS_CLASS.F08);

    const SourceCodeViewers = SourceCodesElement.getElementsByClassName("SourceCodeViewer");
    console.log(SourceCodeViewers[0]);
    SourceCodeViewers[0].getElementsByClassName("SourceCode")[0].id = "CSourceCode";
    SourceCodeViewers[0].classList.add("SourceCodeViewer", "Version", RK.LANGUAGES.C);
    SourceCodeViewers[1].getElementsByClassName("SourceCode")[0].id = "Fortran-90SourceCode";
    SourceCodeViewers[1].classList.add("SourceCodeViewer", "Version", RK.LANGUAGES.F90);
    SourceCodeViewers[2].getElementsByClassName("SourceCode")[0].id = "Fortran-2008SourceCode";
    SourceCodeViewers[2].classList.add("SourceCodeViewer", "Version", RK.LANGUAGES.F08);

    VersionTabContainer.firstChild.click();

    // Generate the Visualisation
    var Visualisation = document.getElementById('Visualisation');
    var VisualisationInnerHTML = "";
    for(var i = 0; i < CellCount; i++)
    {
        VisualisationInnerHTML += "<div id='" + i + "' class='Cell'></div>";
    }
    Visualisation.innerHTML = VisualisationInnerHTML;

    // Add listener to all inputs so that any change triggers display update.
    var AllFields = document.getElementsByTagName('input');
    for(var i = 0; i < AllFields.length; i++)
    {
        AllFields[i].onchange = update_display;
    }

    // Show the form corresponding to the MPI routine selected
    var routineSelected = document.getElementById('routine');
    routineSelected.onchange = pickForm;
    pickForm();
}

window.addEventListener('load', (Event) => {
    OnceWindowIsLoaded();
});

if(document.readyState === "complete") {
    OnceWindowIsLoaded();
}