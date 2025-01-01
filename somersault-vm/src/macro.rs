macro_rules! read {
    ($self: expr, $ty:ty) => {{
        let ip = $self.get_ip();
        let Some(slice) = $self.data.get(ip..ip + std::mem::size_of::<$ty>()) else {
            bail!("Buffer overflow at index {ip}");
        };
        let val = <$ty>::from_le_bytes(slice.try_into()?);
        $self.set_ip((ip + std::mem::size_of::<$ty>()) as i32);
        Ok(val)
    }};
}

macro_rules! param {
    ($i: expr) => {
        super::SCRIPT_PARAMS[$i]
    };
}