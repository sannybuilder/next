import { Injectable } from '@angular/core';

@Injectable({ providedIn: 'root' })
export class FsService {
  async getNewFileHandle() {
    const options: SaveFilePickerOptions = {
      types: [
        {
          description: 'Text Files',
          accept: {
            'text/plain': ['.txt'],
          },
        },
      ],
    };
    const handle = await window.showSaveFilePicker(options);
    return handle;
  }

  async writeFile(fileHandle: FileSystemFileHandle, contents: string) {
    const writable = await fileHandle.createWritable();
    await writable.write(contents);
    await writable.close();
  }
}
