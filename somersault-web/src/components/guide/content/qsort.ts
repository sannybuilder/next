export default `
function quickSort(arr: pint32, len: int)
    if len <= 1 then 
        return
    end

    sort(arr, 0, len - 1)
    
    function partition(arr: pint32, start: int, last: int):int
        int pivot = arr[last]

        int i = start
        int j = start
        while j < last 
            if arr[j] <= pivot then
                arr[i], arr[j] = arr[j], arr[i]
                i += 1
            end
            j += 1
        end
        
        arr[last], arr[i] = arr[i], arr[last]
        return i
    end

    function sort(arr: pint32, start: int, last: int)
        if start >= last then 
            return
        end
        int pivot = partition(arr, start, last)
        sort(arr, start, pivot - 1)
        sort(arr, pivot + 1, last)
    end
end

int arr[10] = 3, 1, 4, 1, 5, 9, 2, 6, 5, 3
quickSort(get_var_pointer(arr), 10)
wait(0)
print_help_formatted("%d %d %d %d %d %d %d %d %d %d", arr[0],arr[1], arr[2], arr[3], arr[4], arr[5], arr[6], arr[7], arr[8], arr[9])


`